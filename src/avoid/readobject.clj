(ns avoid.readobject
  (:require [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.conditionfns :as conditionfns]
            [avoid.addfns :as addfns]
            [avoid.util :as util]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(defn resolve-symbol [x]
  ((comp (partial ns-resolve 'avoid.readobject) symbol) x))

(defn read-template [json-filename]
  (let
   [value-fn (fn [key value]
               (cond
                 (= key :update-fns) (map resolve-symbol value)
                 (= key :shape) (keyword value)
                 (and (string? value) (string/ends-with? value ".json")) (read-template value)
                 :else value))]
    (json/read-str
     (slurp json-filename)
     :key-fn keyword
     :value-fn value-fn)))