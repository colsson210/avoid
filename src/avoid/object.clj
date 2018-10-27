(ns avoid.object
  (:require [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.util :as util]
            [clojure.data.json :as json]))


(defn resolve-symbol [x]
  (println x (symbol x) (resolve (symbol x)) *ns*)
  ((comp resolve symbol) x))

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
  (apply (partial merge {:id (gensym)}) object-fields))

(defn create-player []
  (create player-initial-state player))

(defn create-circle [position radius]
  (create
   (merge
    {:position position :radius radius :color [255 100 100]}
    falling-circle)))

(defn add-random-circle [game-size objects]
  (if (>= (count objects) 10)
    objects
    (let [radius 10 color [255 100 255]
          position (util/find-opening-for-circle game-size radius objects)]
      (if (some? position)
        (cons (create-circle position radius) objects)
        objects))))