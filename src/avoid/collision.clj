(ns avoid.collision
  (:require [avoid.util :as util]
            [clojure.math.combinatorics :as combinatorics]))

(defn distance-line-circle [circle-position circle-radius line-from line-to]
  (let [line (util/vector-minus line-to line-from)
        c (util/vector-minus circle-position line-from)
        ct (min 1 (max 0 (/ (util/dot-product line c) (util/square (util/magnitude line)))))
        dp (util/magnitude
            (util/vector-minus
             (util/vector-plus line-from (util/scalar-vector-multiplication ct line))
             circle-position))
        d (- dp circle-radius)]
    d))

(defn collision-time-line-circle-2 [{line-from :from line-to :to line-direction :direction}
                                    {circle-position :position circle-direction :direction circle-radius :radius}]
  (let [line (util/vector-minus line-to line-from)
        normalized-line (util/normalize line)
        c (util/vector-minus circle-position line-from)
        a (util/scalar-vector-multiplication (util/dot-product c normalized-line) line) ; collision-point
        b (util/vector-minus c a)
        [ax ay] a
        [fx fy] line-from
        [tx ty] line-to
        ctx (if (not= tx fx) (/ (- ax fx) (- tx fx)))
        cty (if (not= ty fy) (/ (- ay fy) (- ty fy)))
        ctxx (if (and (some? ctx) (>= ctx 0.0)) ctx)
        ctyy (if (and (some? cty) (>= cty 0.0)) cty)
        ct (if (and ctxx ctyy) (min ctxx ctyy) (or ctxx ctyy))
        a-on-line? (< (util/magnitude a) (util/magnitude line))]
    (if (and (some? ct) (< ct 100))
      (println ct a-on-line?))
    ct))

(defn collision-time-line-circle [{line-from :from line-to :to line-direction :direction}
                                  {circle-position :position circle-direction :direction circle-radius :radius}]
  (let
   [times (range 0 1 0.01)]
    (some
     (fn [time]
       (let [distance-at-time
             (distance-line-circle
              (util/vector-plus
               circle-position
               (util/scalar-vector-multiplication time circle-direction))
              circle-radius
              (util/vector-plus
               line-from
               (util/scalar-vector-multiplication time line-direction))
              (util/vector-plus
               line-to
               (util/scalar-vector-multiplication time line-direction)))]
         (and (< distance-at-time 0.01) time)))
     times)))

(defn collision-time-polygon-circle [{points :points direction :direction :as polygon} circle]
  (let [lines (map (fn [from to] {:from from :to to :direction direction}) points (concat (rest points) (list (first points))))]
    (let [cts (filter some? (map #(collision-time-line-circle % circle) lines))]
      (if (not (empty? cts)) (apply min cts)))))

(defn collision-time-circles [{[px1 py1] :position [vx1 vy1] :direction r1 :radius}
                              {[px2 py2] :position [vx2 vy2] :direction r2 :radius}]
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
        (if (pos? t1) t1 (if (pos? t2) t2))))))

(defn collision-time [{shape1 :shape :as object1} {shape2 :shape :as object2}]
  (cond
    (and (= shape1 :circle) (= shape2 :circle)) (collision-time-circles object1 object2)
    (and (= shape1 :circle) (= shape2 :line)) (collision-time-line-circle object2 object1)
    (and (= shape1 :circle) (= shape2 :polygon)) (collision-time-polygon-circle object2 object1)
    (= shape2 :circle) (collision-time object2 object1)))

(defn get-collision-object
  ([objects object] (get-collision-object objects object 0.01))
  ([objects object collision-distance]
   (some
    (fn [other-object]
      (let [t (collision-time object other-object)]
        (and
         (some? t)
         (< t 1.0)
         other-object)))
    (util/without-object objects object))))

(defn find-earliest-collision-time [objects]
  (reduce
   (fn [earliest [a b]]
     (let [t (collision-time a b)]
       (if (or (not earliest) (and t (< t earliest))) t earliest)))
   nil
   (combinatorics/combinations objects 2)))

(defn edge-collision-time-circle [[width height] {[px py] :position [vx vy] :direction r :radius}]
  (let
   [tx (if (> (Math/abs vx) 0)
         [(/ (- r px) vx) (/ (- (- width px) r) vx)])
    ty (if (> (Math/abs vy) 0)
         [(/ (- r py) vy) (/ (- (- height py) r) vy)])
    possible-times (filter util/pos-number? (concat tx ty))]
    (if (> (count possible-times) 0)
      (apply min possible-times))))

(defn edge-collision-time-point [[width height] [x y] [dx dy]]
  (let [tx (if (> (Math/abs dx) 0.0) [(- (/ x dx)) (/ (- width x) dx)])
        ty (if (> (Math/abs dy) 0.0) [(- (/ y dy)) (/ (- height y) dy)])
        possible-times (filter util/pos-number? (concat tx ty))]
    (if (> (count possible-times) 0)
      (apply min possible-times))))

(defn edge-collision-time-line [game-size {:keys [from to direction]}]
  (let [f-time (edge-collision-time-point game-size from direction)
        t-time (edge-collision-time-point game-size to direction)]
    (if (and f-time t-time) (min f-time t-time) (or f-time t-time))))

(defn edge-collision-time [game-size {:keys [shape] :as object}]
  (cond
    (= shape :circle) (edge-collision-time-circle game-size object)
    (= shape :line) (edge-collision-time-line game-size object)))

(defn find-earliest-edge-collision [game-size objects]
  (let [edge-collision-times (filter (every-pred some? util/pos-number?) (map (partial edge-collision-time game-size) objects))]
    (if (> (count edge-collision-times) 0) (apply min edge-collision-times))))