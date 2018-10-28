(ns avoid.game
  (:require [avoid.readobject :as readobject]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(defn value-fn [key value]
  (if (and (string? value) (string/ends-with? value ".json"))
    (readobject/read-template value)
    (if (string/ends-with? (str key) "-fn")
      (readobject/resolve-symbol value)
      value)))

(defn read-template [json-filename]
  (json/read-str
   (slurp json-filename)
   :key-fn keyword
   :value-fn value-fn))


