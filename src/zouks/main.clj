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

(defn error [msg parser]
  (throw (ex-info msg parser)))

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

(def keyword-tokens
  {"true" (make-token :boolean true)
   "false" (make-token :boolean false)
   "null" (make-token :null nil)})

(defn tokenize-as-keyword
  [{:keys [index chars]}]
  (str/join
   (take-while #(and (not (Character/isWhitespace %))
                     (not (single-character-tokens %)))
               (subvec chars index))))

(defn starts-with-keyword?
  [data]
  (contains? keyword-tokens (tokenize-as-keyword data)))

(defn lex-keyword
  [data]
  (let [kw (tokenize-as-keyword data)]
    (-> data
        (update :tokens conj (keyword-tokens kw))
        (update :index + (count kw)))))

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

          (Character/isDigit ch) (lex-number data)

          (starts-with-keyword? data) (lex-keyword data)

          :else (error (format "Unexpected token `%s` at index: %s"
                               (nth chars index)
                               index)
                       data)))
      (-> data
          (update :tokens conj (make-token :eof nil))
          (assoc :done? true)))))

(defn lex
  [s]
  (->> {:index 0 :tokens [] :chars (vec (seq s))}
       (iterate lex-token)
       (drop-while #(not (:done? %)))
       first
       :tokens))

(defn expect-token-type
  [parser expected-token-type]
  (let [token-type (:token-type (first (:tokens parser)))]
    (if (= expected-token-type token-type)
      (update parser :tokens next)
      (error (format "Expected token-type `%s`, but got `%s`"
                     expect-token-type
                     token-type)
             parser))))

(defn parse-key
  [parser]
  (let [token (first (:tokens parser))]
    (-> parser
        (expect-token-type :string)
        (assoc :key (:value token)))))

(declare parse-value)

(defn parse-list
  [parser]
  (loop [parser (expect-token-type parser :left-square-bracket)
         elements []]
    (let [token (first (:tokens parser))]
      (cond
        (= :right-square-bracket (:token-type token))
        (-> parser
            (expect-token-type :right-square-bracket)
            (assoc :value (vec elements)))

        (and (seq (:elements parser)) (= :comma (:token-type token)))
        (recur (update parser :tokens next) elements)

        (#{:boolean :number :null :string :left-square-bracket}
         (:token-type token))
        (let [parser-with-value (parse-value parser)]
          (recur (->
                  parser-with-value
                  (update :elements (fnil conj []) (:value parser-with-value))
                  (dissoc :value))
                 (conj elements (:value parser-with-value))))

        :else (error (format "Unexpected token type `%s`"
                             (:token-type token))
                     parser)))))

(defn parse-value
  [parser]
  (let [token (first (:tokens parser))]
    (cond
      (#{:string :boolean :null :number} (:token-type token))
      (-> parser
          (update :tokens next)
          (assoc :value (:value token)))

      (= :left-square-bracket (:token-type token)) (parse-list parser)
      :else (error (format "Unsupported value type: %s" (:token-type token))
                   parser))))

(defn parse-kv
  [parser]
  (let [with-kv (-> parser
                    parse-key
                    (expect-token-type :colon)
                    parse-value)]
    (-> with-kv
        (update :mappings conj [(:key with-kv) (:value with-kv)])
        (dissoc :key)
        (dissoc :value))))

(defn parse-object
  [parser]
  (loop [parser (expect-token-type parser :left-brace)]
    (let [tok-type (:token-type (first (:tokens parser)))]
      (cond
        (= tok-type :right-brace)
        (reduce (fn [m [k v]] (assoc m k v)) {} (:mappings parser))

        (= tok-type :string) (recur (parse-kv parser))

        (and (seq (:mappings parser)) (= tok-type :comma))
        (recur (parse-kv (expect-token-type parser :comma)))

        :else (let [unexpected-token (:value (first (:tokens parser)))]
                (error (format "Unexpected token: `%s`" unexpected-token)
                       parser))))))

(defn parse
  [s]
  (let [tokens (lex s)]
    (if (:error tokens)
      (select-keys tokens [:error])
      (parse-object {:tokens tokens :mappings []}))))

(defn valid-json? [s]
  (try
    (parse s)
    true
    (catch Exception e false)))

(defn parse-and-exit [s] (if (valid-json? s) {:exit 0} {:exit 1}))

(defn -main [& args] (println "Hello World!"))
