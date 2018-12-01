(ns avoid.boundingbox
  (:require [avoid.util :as util]))

(defn get-points-bounding-box [points]
  (let
   [xs (map first points)
    ys (map second points)]
    {:min-x (apply min xs) :max-x (apply max xs) :min-y (apply min ys) :max-y (apply max ys)}))

(defn get-circle-bounding-boxes [{[x y] :position radius :radius}]
  [{:min-x (- x radius) :max-x (+ x radius) :min-y (- y radius) :max-y (+ y radius)}])

(defn get-line-bounding-boxes [{:keys [from to]}]
  [(get-points-bounding-box [from to])])

(defn get-shape-coll-polygon-bounding-boxes [shape-coll-polygon]
  (let [points (util/get-shape-coll-polygon-points shape-coll-polygon)]
    [(get-points-bounding-box points)]))

(defn get-bounding-boxes [{:keys [shape type shapes] :as object}]
  (cond
    (= shape :circle) (get-circle-bounding-boxes object)
    (= shape :line) (get-line-bounding-boxes object)
    (and (= shape :shape-coll) (= type "shape-coll-polygon"))
    (get-shape-coll-polygon-bounding-boxes object)
    (= shape :shape-coll) (mapcat get-bounding-boxes shapes)))

(defn bounding-boxes-overlapping? [{min-x-a :min-x max-x-a :max-x min-y-a :min-y max-y-a :max-y}
                                   {min-x-b :min-x max-x-b :max-x min-y-b :min-y max-y-b :max-y}]
  (not
   (or
    (< max-x-a min-x-b)
    (> min-x-a max-x-b)
    (< max-y-a min-y-b)
    (> min-y-a max-y-b))))

(defn shape-overlapping-bounding-box? [bounding-box shape]
  (println [bounding-box shape])
  (some
   (partial bounding-boxes-overlapping? bounding-box)
   (get-bounding-boxes shape)))

(defn shapes-overlapping? [o1 o2]
  (let [bounding-boxes1 (get-bounding-boxes o1)
        bounding-boxes2 (get-bounding-boxes o2)]
    (some
     (fn [bounding-box1]
       (some (partial bounding-boxes-overlapping? bounding-box1) bounding-boxes2))
     bounding-boxes1)))