(ns avoid.object
  (:require [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.util :as util]
            [clojure.data.json :as json]))

(defn resolve-symbol [x]
  ; (println x (symbol x) (ns-resolve 'avoid.object (symbol x)) *ns*)
  ((comp (partial ns-resolve 'avoid.object) symbol) x))

(defn read-template [json-filename]
  (json/read-str
   (slurp json-filename)
   :key-fn keyword
   :value-fn (fn [key value] (if (= key :update-fns) (map resolve-symbol value) value))))

(def falling-circle (read-template "/home/christian/avoid/src/avoid/objects/falling-circle.json"))
(def player (read-template "/home/christian/avoid/src/avoid/objects/player.json"))
(def player-initial-state (read-template "/home/christian/avoid/src/avoid/objects/player-initial-state.json"))

(println falling-circle)
(println player)
(println player-initial-state)

(defn create [& object-fields]
  (let [object (apply (partial merge {:id (gensym) :color [255 255 255]}) object-fields)]
    (assert (every? (partial contains? object) [:id :color :position :direction :radius :update-fns]))
    object))

(defn create-player []
  (create player-initial-state player))

(defn add-random-circle [game-size objects]
  (if (>= (count objects) 10)
    objects
    (let [radius 10
          position (util/find-opening-for-circle game-size radius objects)]
      (if (some? position)
        (cons
         (create falling-circle {:position position :radius radius})
         objects)
        objects))))