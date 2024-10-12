(ns zouks.main
  (:require
   [clojure.string :as str]))

(defn make-token
  [token-type value]
  {:token-type token-type
   :value value})

(defn lex-string
  [{:keys [index chars] :as data}]
  (loop [end-index (inc index)]
    (if (= \" (nth chars end-index))
      (-> data
          (update :tokens
                  conj
                  (make-token :string
                              (str/join (subvec chars (inc index) end-index))))
          (assoc :index (inc end-index)))
      (recur (inc end-index)))))

(defn lex-number
  [{:keys [index chars] :as data}]
  (let [[digits more] (split-with #(Character/isDigit %) (subvec chars index))
        token (make-token :number (parse-long (str/join digits)))]
    (-> data
        (update :tokens conj token)
        (assoc :chars more))))

(defn add-token-and-advance
  [data token]
  (-> data
      (update :tokens conj token)
      (update :index inc)))

(defn error [msg] {:error msg})

(defn lex-next-token
  [{:keys [index chars] :as data}]
  (let [ch (nth chars index)]
    (cond
      (= ch \{) (add-token-and-advance data (make-token :left-brace "{"))
      (= ch \}) (add-token-and-advance data (make-token :right-brace "}"))
      (= ch \") (lex-string data)
      (= ch \:) (add-token-and-advance data (make-token :colon ":"))
      (= ch \,) (add-token-and-advance data (make-token :comma ","))
      (= ch \t) (let [expected "true"]
                  (when (= expected
                           (str/join
                            (subvec chars index (+ index (count expected)))))
                    (-> data
                        (update :tokens conj (make-token :boolean true))
                        (update :index + (count expected)))))
      (= ch \f) (let [expected "false"]
                  (when (= expected
                           (str/join
                            (subvec chars index (+ index (count expected)))))
                    (-> data
                        (update :tokens conj (make-token :boolean false))
                        (update :index + (count expected)))))
      (= ch \n) (let [expected "null"]
                  (when (= expected
                           (str/join
                            (subvec chars index (+ index (count expected)))))
                    (-> data
                        (update :tokens conj (make-token :null nil))
                        (update :index + (count expected)))))
      (Character/isDigit ch) (lex-number data)
      :else (error (format "Unexpected token `%s` at index: %s"
                           (nth chars index)
                           index)))))

(defn skip-whitespace
  [{:keys [index chars] :as data}]
  (cond
    (:error data) data
    (and (< index (count chars)) (Character/isWhitespace (nth chars index)))
    (recur (update data :index inc))

    :else data))

(defn eof? [{:keys [index chars]}] (<= (count chars) index))

(defn lex-token
  [data]
  (let [data (skip-whitespace data)]
    (cond
      (:error data) (assoc data :done? true)
      (eof? data) (-> data
                      (update :tokens conj (make-token :eof nil))
                      (assoc :done? true))
      :else (lex-next-token data))))

(defn lex
  [s]
  (let [result (->> {:index 0 :tokens [] :chars (vec (seq s))}
                    (iterate lex-token)
                    (drop-while #(not (:done? %)))
                    first)]
    (if (:error result) result (:tokens result))))

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
        (if (#{:string :boolean :null} (:token-type token))
          (recur (-> parser
                     (update :tokens next)
                     (update :mappings conj [(:key parser) (:value token)])
                     (assoc :state :comma-or-end-of-object)))
          (error (format "Unsupported value type: %s" (:token-type token)))))

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
                         (error "Expected opening brace"))
      (= state :end-of-object)
      (if (= :right-brace (:token-type (first tokens)))
        (reduce (fn [m [k v]] (assoc m k v)) {} mappings)
        (error "Expected closing brace"))

      :else (error (format "Unknown state: `%s`" state)))))

(defn parse [s] (parse-tokens (lex s)))

(defn valid-json? [s] (not (:error (parse s))))

(defn parse-and-exit [s] (if (valid-json? s) {:exit 0} {:exit 1}))

(defn -main [& args] (println "Hello World!"))

