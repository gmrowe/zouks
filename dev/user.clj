(ns user
  (:require
   [clojure.tools.namespace.repl :refer [refresh]]
   [kaocha.repl :as k]))

(defn test-all [] (refresh) (k/run :unit))
