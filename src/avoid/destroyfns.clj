(ns avoid.destroyfns
  (:require [avoid.destroy :as destroy]
            [avoid.collision :as collision]
            [avoid.util :as util]))

(def destroy-on-exit-bottom
  (destroy/create (<= y radius)))

(def destroy-on-collision
  (destroy/create (collision/get-collision-object other-objects object)))

(def destroy-on-edge-collision
  (destroy/create
   (let [collision-object (collision/get-collision-object other-objects object)]
    (= (:type collision-object) "edge"))))

