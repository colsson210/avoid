(ns avoid.cavesegment
  (:require [avoid.util :as util]))

(defn get-cave-segments [state]
  (filter (comp (partial = "cave-segment") :type) state))

(defn get-cave-segment-element-points [cave-segment-element]
  (mapcat (fn [{:keys [from to]}] [from to]) (:shapes cave-segment-element)))

(defn get-cave-segment-points [state]
  (mapcat (comp (partial mapcat get-cave-segment-element-points) :shapes) (get-cave-segments state)))

(defn get-cave-segment-elements [state]
  (let [cave-segments (get-cave-segments state)
        shapes (mapcat :shapes cave-segments)]
    shapes))

(defn get-cave-segment-elements-at-x [x objects]
  (let [cave-segment-elements (get-cave-segment-elements objects)]
    (filter
     (fn [{:keys [shapes]}]
       (some (fn [{[fx fy] :from [tx ty] :to}] (or (= fx x) (= tx x))) shapes))
     cave-segment-elements)))

(defn get-cave-segment-max-x [state]
  (let [points (get-cave-segment-points state)
        xs (map first points)]
    (if (empty? xs) 0 (apply max xs))))

(defn get-rightmost-cave-segment [state]
  (->>
   state
   get-cave-segments
   (reduce
    (fn [{end-x-a :end-x :as a} {end-x-b :end-x :as b}]
      (if (> end-x-b end-x-a) b a)))))