(ns avoid.collision
  (:require [avoid.util :as util]
            [clojure.math.combinatorics :as combinatorics]))

(defn collision-times [[px1 py1] [vx1 vy1] r1 [px2 py2] [vx2 vy2] r2]
  (let [cx (- px2 px1)
        cy (- py2 py1)
        vx (- vx2 vx1)
        vy (- vy2 vy1)
        r (+ r1 r2)
        r-square (util/square r)
        v-squares-sum (+ (util/square vx) (util/square vy))
        b-sqrt-input (- (* r-square v-squares-sum) (util/square (- (* cx vy) (* cy vx))))]
    (if (and (> v-squares-sum 0.0) (> b-sqrt-input 0.0))
      (let [a (* -1.0 (+ (* cx vx) (* cy vy)))
            b (Math/sqrt b-sqrt-input)
            c (/ 1.0 v-squares-sum)
            t1 (* c (- a b))
            t2 (* c (+ a b))]
        [t1 t2]))))

(defn min-pos-collision-time [{a-position :position a-direction :direction a-radius :radius} {b-position :position b-direction :direction b-radius :radius}]
  (let [c-ts (collision-times
              a-position a-direction a-radius
              b-position b-direction b-radius)]
    (if (some? c-ts)
      (let [positive-collision-times (filter pos? c-ts)]
        (if (not (empty? positive-collision-times))
          (apply min positive-collision-times))))))

(defn collision? [& circles]
  (let [sides-length (apply + (apply (partial map (comp util/square -)) (map :position circles)))
        h-length (apply (comp util/square +) (map :radius circles))]
    (<= (- sides-length h-length) 0.0)))

(defn get-collision-object
  ([objects object] (get-collision-object objects object 0.01))
  ([objects object collision-distance]
   (some
    (fn [other-object]
      (let [t (min-pos-collision-time object other-object) d (util/distance object other-object)]
        (and
         (< (util/distance object other-object) collision-distance)
                   ; (collision? object other-object)
                  ; t
                  ;  (<= t 0.0)
         other-object)))
    (util/without-object objects object))))

(defn find-earliest-collision-time [objects]
  (reduce
   (fn [earliest [a b]]
     (let [t (min-pos-collision-time a b)]
       (if (or (not earliest) (and t (< t earliest))) t earliest)))
   nil
   (combinatorics/combinations objects 2)))

(def pos-number? (comp some? pos?))

(defn edge-collision-time [[width height] {[px py] :position [vx vy] :direction r :radius}]
  (let
   [tx (if (> (Math/abs vx) 0)
         [(/ (- r px) vx) (/ (- (- width px) r) vx)])
    ty (if (> (Math/abs vy) 0)
         [(/ (- r py) vy) (/ (- (- height py) r) vy)])
    possible-times (filter pos-number? (concat tx ty))]
    (if (> (count possible-times) 0)
      (apply min possible-times))))

(defn find-earliest-edge-collision [game-size objects]
  (let [edge-collision-times (filter pos-number? (map (partial edge-collision-time game-size) objects))]
    (if (> (count edge-collision-times) 0) (apply min edge-collision-times))))