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
                  {:token-type :string
                   :value (str/join (subvec chars (inc index) end-index))})
          (assoc :index (inc end-index)))
      (recur (inc end-index)))))

(defn add-token-and-advance
  [data token]
  (-> data
      (update :tokens conj token)
      (update :index inc)))

(declare lex-token)

(defn lex-next-token
  [{:keys [index chars] :as data}]
  (case (nth chars index)
    \{ (add-token-and-advance data (make-token :left-brace "{"))
    \} (add-token-and-advance data (make-token :right-brace "}"))
    \" (lex-string data)
    \: (add-token-and-advance data (make-token :colon ":"))
    \, (add-token-and-advance data (make-token :comma ","))))

(defn skip-whitespace
  [{:keys [index chars] :as data}]
  (if (and (< index (count chars)) (Character/isWhitespace (nth chars index)))
    (recur (update data :index inc))
    data))

(defn eof? [{:keys [index chars]}] (<= (count chars) index))

(defn error [msg] {:error msg})

(defn lex-token
  [data]
  (let [data (skip-whitespace data)]
    (cond
      (eof? data) (-> data
                      (update :tokens conj {:token-type :eof})
                      (assoc :done? true))
      :else (lex-next-token data))))

(defn lex
  [s]
  (let [data {:index 0 :tokens [] :chars (vec (seq s))}]
    (->> data
         (iterate lex-token)
         (drop-while #(not (:done? %)))
         first
         :tokens)))

(defn parse
  [s]
  (loop [tokens (lex s)
         state :init
         mappings []]
    (cond
      (= state :init) (if (= :left-brace (:token-type (first tokens)))
                        (recur (next tokens) :kv-or-end-of-object mappings)
                        (error "Expected opening brace"))
      (= state :kv-or-end-of-object)
      (let [tok-type (:token-type (first tokens))]
        (cond
          (= tok-type :right-brace) (recur tokens :end-of-object mappings)
          (= tok-type :string) (recur tokens :kv mappings)
          :else (error "Expected kv pair or closing brace")))

      (= state :kv) (if (= [:string :colon :string]
                           (map :token-type (take 3 tokens)))
                      (let [[k _ v] (map :value (take 3 tokens))]
                        (recur (drop 3 tokens)
                               :next-mapping-or-end-of-object
                               (conj mappings [k v])))
                      (error "Expected kv pair"))
      (= state :next-mapping-or-end-of-object)
      (let [tok-type (:token-type (first tokens))]
        (cond
          (= tok-type :right-brace) (recur tokens :end-of-object mappings)
          (= tok-type :comma) (recur (next tokens) :kv mappings)
          :else (error "Expected kv pair or closing brace")))

      (= state :end-of-object)
      (if (= :right-brace (:token-type (first tokens)))
        (reduce (fn [m [k v]] (assoc m k v)) {} mappings)
        (error "Expected closing brace"))

      :else (:error "Unrecognized state"))))

(defn valid-json? [s] (not (:error (parse s))))

(defn parse-and-exit [s] (if (valid-json? s) {:exit 0} {:exit 1}))

(defn -main [& args] (println "Hello World!"))

