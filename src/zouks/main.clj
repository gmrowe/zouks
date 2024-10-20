(ns zouks.main
  (:require
   [clojure.string :as str]))

(defn make-token
  [token-type value]
  {:token-type token-type
   :value value})

(defn lex-string
  [{:keys [index chars] :as data}]
  (let [string (take-while #(not= \" %) (subvec chars (inc index)))
        token (make-token :string (str/join string))]
    (-> data
        (update :tokens conj token)
        (update :index + (count string) 2))))

(defn lex-number
  [{:keys [index chars] :as data}]
  (let [digits (take-while #(Character/isDigit %) (subvec chars index))
        token (make-token :number (parse-long (str/join digits)))]
    (-> data
        (update :tokens conj token)
        (update :index + (count digits)))))

(defn error [msg] {:error msg})

(defn skip-whitespace
  [{:keys [index chars] :as data}]
  (if (and (< index (count chars)) (Character/isWhitespace (nth chars index)))
    (recur (update data :index inc))
    data))

(defn eof? [{:keys [index chars]}] (<= (count chars) index))

(def single-character-tokens
  {\{ (make-token :left-brace "{")
   \} (make-token :right-brace "}")
   \: (make-token :colon ":")
   \, (make-token :comma ",")
   \[ (make-token :left-square-bracket "[")
   \] (make-token :right-square-bracket "]")})

(defn lex-token
  [data]
  ;; TODO: do we really need indexed based parsing or should we
  ;;       move to more list based parsing?
  (let [{:keys [index chars] :as data} (skip-whitespace data)]
    (if (< index (count chars))
      (let [ch (nth chars index)]
        (cond
          (some? (single-character-tokens ch))
          (-> data
              (update :tokens conj (single-character-tokens ch))
              (update :index inc))

          (= ch \") (lex-string data)
          (= ch \t)
          (let [expected "true"]
            (when (= expected
                     (str/join (subvec chars index (+ index (count expected)))))
              (-> data
                  (update :tokens conj (make-token :boolean true))
                  (update :index + (count expected)))))

          (= ch \f)
          (let [expected "false"]
            (when (= expected
                     (str/join (subvec chars index (+ index (count expected)))))
              (-> data
                  (update :tokens conj (make-token :boolean false))
                  (update :index + (count expected)))))

          (= ch \n)
          (let [expected "null"]
            (when (= expected
                     (str/join (subvec chars index (+ index (count expected)))))
              (-> data
                  (update :tokens conj (make-token :null nil))
                  (update :index + (count expected)))))

          (Character/isDigit ch) (lex-number data)
          :else (error (format "Unexpected token `%s` at index: %s"
                               (nth chars index)
                               index))))
      (-> data
          (update :tokens conj (make-token :eof nil))
          (assoc :done? true)))))

(defn lex
  [s]
  (let [result (->> {:index 0 :tokens [] :chars (vec (seq s))}
                    (iterate lex-token)
                    (drop-while #(and (not (:done? %)) (not (:error %))))
                    first)]
    (if (:error result) result (:tokens result))))

(defn expect-token-type
  [parser expected-token-type]
  (let [token-type (:token-type (first (:tokens parser)))]
    (if (= expected-token-type token-type)
      (update parser :tokens next)
      (error (format "Expected token-type `%s`, but got `%s`"
                     expect-token-type
                     token-type)))))

(defn parse-key
  [parser]
  (let [token (first (:tokens parser))]
    (if (= :string (:token-type token))
      (-> parser
          (update :tokens next)
          (assoc :key (:value token)))
      (error "Expected string - key must be a string"))))

(defn parse-list
  [parser]
  (loop [parser (expect-token-type parser :left-square-bracket)]
    (let [token (first (:tokens parser))]
      (-> parser
          (expect-token-type :right-square-bracket)
          (update :mappings conj [(:key parser) []])))))

(defn parse-value
  [parser]
  (let [token (first (:tokens parser))]
    (cond
      (#{:string :boolean :null :number} (:token-type token))
      (-> parser
          (update :tokens next)
          (update :mappings conj [(:key parser) (:value token)]))

      (= :left-square-bracket (:token-type token)) (parse-list parser)
      :else (error (format "Unsupported value type: %s" (:token-type token))))))

(defn parse-kv
  [parser]
  (-> parser
      parse-key
      (expect-token-type :colon)
      parse-value))

(defn parse-object
  [parser]
  (loop [parser (expect-token-type parser :left-brace)]
    (let [tok-type (:token-type (first (:tokens parser)))]
      (cond
        (= tok-type :right-brace)
        (reduce (fn [m [k v]] (assoc m k v)) {} (:mappings parser))

        (= tok-type :string) (recur (parse-kv parser))
        (and (seq (:mappings parser)) (= tok-type :comma))
        (recur (update parser :tokens next))

        :else (let [unexpected-token (:value (first (:tokens parser)))]
                (error (format "Unexpected token: `%s`" unexpected-token)))))))

(defn parse
  [s]
  (let [tokens (lex s)]
    (if (:error tokens)
      (select-keys tokens [:error])
      (parse-object {:tokens tokens :mappings []}))))

(defn valid-json? [s] (not (:error (parse s))))

(defn parse-and-exit [s] (if (valid-json? s) {:exit 0} {:exit 1}))

(defn -main [& args] (println "Hello World!"))
