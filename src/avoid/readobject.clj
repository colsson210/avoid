(ns avoid.readobject
  (:require [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.conditionfns :as conditionfns]
            [avoid.addfns :as addfns]
            [avoid.util :as util]
            [clojure.data.json :as json]))

(defn resolve-symbol [x]
  ((comp (partial ns-resolve 'avoid.readobject) symbol) x))

(defn read-template [json-filename]
  (json/read-str
   (slurp json-filename)
   :key-fn keyword
   :value-fn (fn [key value] (if (= key :update-fns) (map resolve-symbol value) value))))