(ns avoid.game
  (:require [avoid.object :as object]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(defn resolve-symbol [x]
  ((comp (partial ns-resolve 'avoid.game) symbol) x))

(defn value-fn [key value]
  (if (and (string? value) (string/ends-with? value ".json"))
    (object/read-template value)
    value))

(defn read-template [json-filename]
  (json/read-str
   (slurp json-filename)
   :key-fn keyword
   :value-fn value-fn))

; (def avoid-falling (read-template "/home/christian/avoid/src/avoid/games/avoid-falling.json"))

; (println avoid-falling)

