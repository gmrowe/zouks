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
                   :value (str/join (subvec chars index (inc end-index)))})
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
    \: (add-token-and-advance data (make-token :colon ":"))))

(defn skip-whitespace
  [{:keys [index chars] :as data}]
  (if (and (< index (count chars)) (Character/isWhitespace (nth chars index)))
    (recur (update data :index inc))
    data))

(defn eof? [{:keys [index chars]}] (<= (count chars) index))

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

(defn valid-json?
  [s]
  (= (map :token-type (lex s)) [:left-brace :right-brace :eof]))

(defn parse-and-exit [s] (if (valid-json? (lex s)) {:exit 0} {:exit 1}))

(defn -main [& args] (println "Hello World!"))
