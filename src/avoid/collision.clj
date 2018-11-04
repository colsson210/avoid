(ns avoid.collision
  (:require [avoid.util :as util]
            [clojure.math.combinatorics :as combinatorics]))

(defn ct-cl-helper [cp lf lt]
  (let
   [cc (- lf cp)
    c (* cc cc)
    b (+ (* lf lt) (- (* lf lf)) (* cp lf) (- (* c lt)))
    aa (- lf lt)
    a (* cc cc)]
    {:a a :b b :c c}))

(defn distance-l-p-t [fx fy tx ty px py l]
  (let [x (- (+ fx (* l (- tx fx))) px)
        y (- (+ fy (* l (- ty fy))) py)]
    (Math/sqrt (+ (* x x) (* y)))))

(defn distance-line-circle [[cpx cpy] cr [lfx lfy] [ltx lty]]
  (let
   [{ax :a bx :b cx :c} (ct-cl-helper cpx lfx ltx)
    {ay :a by :b cy :c} (ct-cl-helper cpy lfy lty)
    a (+ ax ay)
    b (+ bx by)
    c (- (+ cx cy) (* cr cr))
    l0 (/ (- (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))
    l1 (/ (+ (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))
    l (min (max l0 l1 0) 1.0)]
    (distance-l-p-t lfx lfy ltx lty cpx cpy l)))

(defn collision-times-line-circle [{line-from :from line-to :to line-direction :direction}
                                   {circle-position :position circle-direction :direction circle-radius :radius}]
  (let
   [times (range 0 1 0.01)]
    (some
     (fn [time]
       (let [distance-at-time
             (distance-line-circle
              (util/vector-plus
               circle-position
               (util/scalar-vector-multiplication time circle-position))
              circle-radius
              (util/vector-plus
               line-from
               (util/scalar-vector-multiplication time line-from))
              (util/vector-plus
               line-to
               (util/scalar-vector-multiplication time line-to)))]
         (and (< distance-at-time 0.01) [time])))
     times))
  false)

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
    (and (= shape1 :line) (= shape2 :circle)) (collision-times-line-circle object1 object2)
    (and (= shape1 :circle) (= shape2 :line)) (collision-time shape2 shape1)))

(defn get-collision-object
  ([objects object] (get-collision-object objects object 0.01))
  ([objects object collision-distance]
   (some
    (fn [other-object]
      (let [t (collision-time object other-object) d (util/distance object other-object)]
        (and
         (< (util/distance object other-object) collision-distance)
         (do (println t) other-object))))
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
  (let [f-times (edge-collision-time-point game-size from direction)
        t-times (edge-collision-time-point game-size to direction)
        edge-collision-times (concat f-times t-times)]
    (if (> (count edge-collision-times) 0) (apply min edge-collision-times))))

(defn edge-collision-time [game-size {:keys [shape] :as object}]
  (cond
    (= shape :circle) (edge-collision-time-circle game-size object)
    (= shape :line) (edge-collision-time-line game-size object)))

(defn find-earliest-edge-collision [game-size objects]
  (let [edge-collision-times (filter util/pos-number? (map (partial edge-collision-time game-size) objects))]
    (if (> (count edge-collision-times) 0) (apply min edge-collision-times))))