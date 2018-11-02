(ns avoid.readobject
  (:require [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.conditionfns :as conditionfns]
            [avoid.addfns :as addfns]
            [avoid.util :as util]
            [clojure.data.json :as json]))

(defn resolve-symbol [x]
  ((comp (partial ns-resolve 'avoid.readobject) symbol) x))

(defn value-fn [key value]
  (cond
    (= key :update-fns) (map resolve-symbol value)
    (= key :draw) (keyword value)
    :else value))

(defn read-template [json-filename]
  (json/read-str
   (slurp json-filename)
   :key-fn keyword
   :value-fn value-fn))