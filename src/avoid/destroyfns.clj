(ns avoid.destroyfns
  (:require [avoid.destroy :as destroy]
            [avoid.collision :as collision]))

(def destroy-on-exit-bottom
  (destroy/create (<= y radius)))

(def destroy-on-collision
  (destroy/create (collision/get-collision-object other-objects object 0.1)))

(def destoy-on-edge-collision
  (destroy/create
   (or
    (<= x radius)
    (<= y radius)
    (>= x (- game-width radius))
    (>= y (- game-height radius)))))