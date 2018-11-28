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

(def destroy-on-cave-max-x-below-0
  (destroy/create
   (->>
    shapes
    (mapcat util/get-cave-segment-element-points)
    (map first)
    (apply max)
    (> 0))))

(def destroy-on-outside-game
  (destroy/create
   (not
    (util/shape-overlapping-bounding-box?
     {:min-x 0 :max-x game-width :min-y 0 :max-y game-height}
     object))))

(def destroy-on-left-of-game
  (destroy/create
   (let [bounding-boxes (util/get-bounding-boxes object)
         total-max-x (reduce
                      (fn [old-max-x {:keys [max-x]}] (if (or (not old-max-x) (> max-x old-max-x)) max-x old-max-x))
                      nil
                      bounding-boxes)]
     (< total-max-x 0))))