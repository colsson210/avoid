(ns avoid.destroyfns
  (:require [avoid.destroy :as destroy]
            [avoid.collision :as collision]))

(def destroy-on-exit-bottom
  (destroy/create
   (let [[x y] position] (<= y radius))))

(def destroy-on-collision
  (destroy/create (collision/get-collision-object other-objects object 0.1)))