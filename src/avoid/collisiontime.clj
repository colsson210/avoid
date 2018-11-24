(comment
  (ns avoid.collisiontime
    (:require [avoid.util :as util]
              [clojure.math.combinatorics :as combinatorics]))
  
  (defn collision-time-line-circle [{line-from :from line-to :to line-direction :direction}
                                    {circle-position :position circle-direction :direction circle-radius :radius}]
    (let
     [times (range 0 1 0.1)]
      (some
       (fn [time]
         (let [distance-at-time
               (util/distance-line-circle
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
           (and (< distance-at-time 0.1) time)))
       times)))
  
  (defn collision-time-line-line [{d1 :direction f1 :from t1 :to :as l1}
                                  {d2 :direction f2 :from t2 :to}]
    (let
     [times (range 0 1 0.1)]
      (some
       (fn [time]
         (let [step1 (util/scalar-vector-multiplication time d1)
               step2 (util/scalar-vector-multiplication time d2)]
           (if
            (collision-line-line?
             (util/vector-plus step1 f1)
             (util/vector-plus step1 t1)
             (util/vector-plus step2 f2)
             (util/vector-plus step2 t2))
             time)))
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
  
  (declare collision-time)
  (defn collision-time-shape-coll-object [shape-coll object]
    (reduce (fn [min-time s]
              (let [ct (collision-time object s)]
                (if (or (not min-time) (and ct (< ct min-time))) ct min-time)))
            nil
            (:shapes shape-coll)))
  
  (defn collision-time [{shape1 :shape :as object1} {shape2 :shape :as object2}]
    (cond
      (and (= shape1 :circle) (= shape2 :circle)) (collision-time-circles object1 object2)
      (and (= shape1 :circle) (= shape2 :line)) (collision-time-line-circle object2 object1)
      (and (= shape1 :circle) (= shape2 :polygon)) (collision-time-polygon-circle object2 object1)
      (and (= shape1 :line) (= shape2 :line)) (collision-time-line-line object1 object2)
      (= shape1 :shape-coll) (collision-time-shape-coll-object object1 object2)
      (or (= shape2 :shape-coll) (= shape2 :circle)) (collision-time object2 object1)))
  
  (defn get-collision-object-using-time
    ([objects object] (get-collision-object objects object 0.01))
    ([objects object collision-distance]
     (some
      (fn [{:keys [shape] :as other-object}]
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
    
  
  )

