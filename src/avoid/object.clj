(ns avoid.object
  (:require [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.util :as util]))

(defn create [& object-fields]
  (apply (partial merge {:id (gensym)}) object-fields))

(defn create-player [color]
  (create
   {:position [100 100] :direction [1 1] :radius 20 :color color
    :collisions 0
    :update-fns [updatefns/handle-player-action
                 updatefns/decrease-direction
                 updatefns/bounce-edges-update
                 updatefns/take-color-on-collision
                 updatefns/collision-counter]}))

(defn create-circle [position radius]
  (create
   {:position position :radius radius :direction [0 -1] :color [255 100 100]
    :update-fns [updatefns/color-by-position
                 destroyfns/destroy-on-exit-bottom
                 destroyfns/destroy-on-collision]}))

(defn add-random-circle [[width height] objects]
  (if (>= (count objects) 10)
    objects
    (let [radius 10 color [255 100 255]
          position (util/find-opening-for-circle [width height] radius objects)]
      (if (some? position)
        (cons (create-circle position radius) objects)
        objects))))