(ns avoid.destroyfns
  (:require [avoid.destroy :as destroy]
            [avoid.collision :as collision]
            [avoid.util :as util]))

(def destroy-on-exit-bottom
  (destroy/create (<= y radius)))

(def destroy-on-collision
  (destroy/create (collision/get-collision-object other-objects object 0.1)))

(def destroy-on-edge-collision
  (destroy/create
   (or
    (<= x radius)
    (<= y radius)
    (>= x (- game-width radius))
    (>= y (- game-height radius)))))

(def polygon-destroy
  (destroy/create
   (or
    (every?
     (fn [from to]
       (util/line-points-within game-size [from to]))
     points))))
