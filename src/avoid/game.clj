(ns avoid.game
  (:require [avoid.readobject :as readobject]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(defn value-fn [key value]
  (if (and (string? value) (string/ends-with? value ".json"))
    (readobject/read-template value)
    (let [str-key (str key)]
    (if (or (string/ends-with? str-key "-fn") (string/ends-with? str-key "-pred"))
      (readobject/resolve-symbol value)
      value))))

(defn read-template [json-filename]
  (json/read-str
   (slurp json-filename)
   :key-fn keyword
   :value-fn value-fn))


