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
   \, (make-token :comma "")})

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


;; TODO: Should we have a function to parse individual tokens or is it ok to
;;       parse all tokens in a loop?
(defn parse-tokens
  [tokens]
  (loop [{:keys [tokens state mappings] :as parser}
         {:tokens tokens :state :init :mappings []}]
    (cond
      (= state :init) (if (= :left-brace (:token-type (first tokens)))
                        (recur (-> parser
                                   (update :tokens next)
                                   (assoc :state :key-or-end-of-object)))
                        (error "Expected opening brace"))
      ;; TODO: Implement a `peek` style function so we don't have to enter
      ;; this
      ;;       state at all
      (= state :key-or-end-of-object)
      (let [tok-type (:token-type (first tokens))]
        (cond
          (= tok-type :right-brace) (recur (assoc parser :state :end-of-object))
          (= tok-type :string) (recur (assoc parser :state :key))
          :else (error "Expected kv pair or closing brace")))

      (= state :key) (let [token (first tokens)]
                       (if (= :string (:token-type token))
                         (recur (-> parser
                                    (update :tokens next)
                                    (assoc :key (:value token))
                                    (assoc :state :separator)))
                         (error "Expected string - key must be a string")))
      (= state :separator)
      (let [tok-type (:token-type (first tokens))]
        (if (= tok-type :colon)
          (recur (-> parser
                     (update :tokens next)
                     (assoc :state :value)))
          (error "Expected colon (`:`) - separator must be colon")))

      (= state :value)
      (let [token (first tokens)]
        (if (#{:string :boolean :null :number} (:token-type token))
          (recur (-> parser
                     (update :tokens next)
                     (update :mappings conj [(:key parser) (:value token)])
                     (assoc :state :comma-or-end-of-object)))
          (error (format "Unsupported value type: %s" (:token-type token)))))

      ;; TODO: Implement a `peek` style function so we don't have to enter
      ;; this state at all
      (= state :comma-or-end-of-object)
      (let [tok-type (:token-type (first tokens))]
        (cond
          (= tok-type :comma) (recur (assoc parser :state :comma))
          (= tok-type :right-brace) (recur (assoc parser :state :end-of-object))
          :else (error "Expected comma or closing brace")))

      (= state :comma) (if (= :comma (:token-type (first tokens)))
                         (recur (-> parser
                                    (update :tokens next)
                                    (assoc :state :key)))
                         (error "Expected comma"))
      (= state :end-of-object)
      (if (= :right-brace (:token-type (first tokens)))
        (reduce (fn [m [k v]] (assoc m k v)) {} mappings)
        (error "Expected closing brace"))

      :else (error (format "Unknown state: `%s`" state)))))

(defn parse
  [s]
  (let [tokens (lex s)]
    (if (:error tokens) (select-keys tokens [:error]) (parse-tokens tokens))))

(defn valid-json? [s] (not (:error (parse s))))

(defn parse-and-exit [s] (if (valid-json? s) {:exit 0} {:exit 1}))

(defn -main [& args] (println "Hello World!"))

