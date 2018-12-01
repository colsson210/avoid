(ns avoid.collision
  (:require [avoid.util :as util]
            [avoid.move :as move]
            [clojure.math.combinatorics :as combinatorics]))

(defn collision-circle-line? [{circle-position :position circle-direction :direction circle-radius :radius}
                              {line-from :from line-to :to line-direction :direction}]
  (let [distance (util/distance-line-circle circle-position circle-radius line-from line-to)]
    (< distance 0.1)))

(defn collision-line-line? [[f1x f1y] [t1x t1y] [f2x f2y] [t2x t2y]]
  (let [[s1x s1y] (util/get-line-direction [f1x f1y] [t1x t1y])
        [s2x s2y] (util/get-line-direction [f2x f2y] [t2x t2y])
        d (+ (* -1 s2x s1y) (* s1x s2y))]
    (if (not (zero? d))
      (let [s (/ (+ (* -1 s1y (- f1x f2x)) (* s1x (- f1y f2y))) d)
            t (/ (- (* s2x (- f1y f2y)) (* s2y (- f1x f2x))) d)]
        (and (>= s 0) (<= s 1) (>= t 0) (<= t 1))))))

(defn collision-circle-circle? [{[x1 y1] :position radius1 :radius} {[x2 y2] :position radius2 :radius}]
  (let [x-diff (- x1 x2)
        y-diff (- y1 y2)
        combined-radius (+ radius1 radius2)]
    (<
     (+ (* x-diff x-diff) (* y-diff y-diff))
     (* combined-radius combined-radius))))

(declare collision?)
(defn collision-shape-coll? [shape-coll shape]
  (some (partial collision? shape) (:shapes shape-coll)))

(defn collision? [{shape1 :shape type1 :type :as object1} {shape2 :shape type2 :type :as object2}]
  (cond
    (= shape1 :shape-coll) (collision-shape-coll? shape1 shape2)
    (= shape2 :shape-coll) (collision? shape2 shape1)
    (and (= shape1 :circle) (= shape2 :circle)) (collision-circle-circle? object1 object2)
    (and (= shape1 :line) (= shape2 :line)) (collision-line-line? (:from object1) (:to object1) (:from object2) (:to object2))
    (and (= shape1 :circle) (= shape2 :line)) (collision-circle-line? object1 object2)
    (= shape2 :circle) (collision? object2 object1)))

(defn some-collision? [objects]
  (some (partial apply collision?) (combinatorics/combinations objects 2)))

(defn get-collision-object [objects object]
  (some
   (fn [{:keys [shape shapes] :as other-object}]
     (if (= shape :shape-coll)
       (get-collision-object shapes object)
       (if (collision? object other-object)
         (do other-object))))
   (util/without-object objects object)))

