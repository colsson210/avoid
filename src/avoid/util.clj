(ns avoid.util (:gen-class))

(defn square [x] (* x x))

(defn dot-product [v1 v2]
  (apply + (apply (partial map *) [v1 v2])))

(defn scalar-vector-multiplication [s v]
  (map (partial * s) v))

(defn vector-plus [& vectors]
  (vec (apply (partial map +) vectors)))

(defn vector-minus [& vectors]
  (apply (partial map -) vectors))

(def magnitude
  (comp (fn [x] (Math/sqrt x)) (partial apply +) (partial map square)))

(defn position-distance [p1 p2]
  (Math/sqrt (apply + (map (comp square -) p1 p2))))

(defn normalize [v]
  (let [m (magnitude v)]
    (if (not= m 0.0)
      (scalar-vector-multiplication (/ 1.0 m) v)
      v)))

(def normalized-direction (comp normalize vector-minus))

(defn collide-circles [p1 v1 p2 v2]
  (let
   [n (normalized-direction p1 p2)
    negative-n (scalar-vector-multiplication -1.0 n)
    vn1 (scalar-vector-multiplication
         (dot-product v1 negative-n) negative-n)
    vn2 (scalar-vector-multiplication
         (dot-product v2 n) n)
    vt1 (vector-minus vn1 v1)
    vt2 (vector-minus vn2 v2)]
    {:v1-next (vector-plus vt1 vn2)
     :v2-next (vector-plus vt2 vn1)}))

(defn without-object [objects {:keys [id]}]
  (filter (comp (partial not= id) :id) objects))

(defn overlapping-any? [other-objects {:keys [id position radius shape]}]
  (some?
   (some (fn [{other-id :id other-position :position other-radius :radius other-shape :shape}]
           (cond
             (and (= shape :circle) (= other-shape :circle))
             (< (position-distance position other-position) (+ radius other-radius))
             :else false))
         (without-object other-objects {:id id}))))

(defn distance [& circles]
  (let [sides-length (apply + (apply (partial map (comp square -)) (map :position circles)))
        h-length (apply (comp square +) (map :radius circles))]
    (- sides-length h-length)))

(defn find-opening-for-circle
  ([[[x-min x-max] [y-min y-max]] radius objects] (find-opening-for-circle [[x-min x-max] [y-min y-max]] radius objects 0))
  ([[[x-min x-max] [y-min y-max]] radius objects attempts]
   (if (and (<= x-min x-max) (<= y-min y-max))
     (let [position
           [(+ x-min radius (rand-int (- x-max (+ x-min (* 2 radius)))))
            (+ y-min radius (rand-int (- y-max (+ y-min (* 2 radius)))))]]
       (if (not (overlapping-any? objects {:position position :radius radius :id (gensym)}))
         position
         (if (< attempts 10)
           (find-opening-for-circle [[x-min x-max] [y-min y-max]] radius objects (inc attempts))))))))

(def pos-number? (comp some? pos?))

(defn point-within [[width height] [x y]]
  (and (>= x 0) (<= x width) (>= y 0) (<= y height)))

(defn line-points-within [size [from to]]
  (and
   (point-within size from)
   (point-within size to)))

(defn distance-line-circle [circle-position circle-radius line-from line-to]
  (let [[fx fy] line-from [tx ty] line-to]
    (assert (not (and (= fx tx) (= fy ty))) "line-from cannot be equal to line-to"))
  (let [line (vector-minus line-to line-from)
        c (vector-minus circle-position line-from)
        ct (min 1 (max 0 (/ (dot-product line c) (square (magnitude line)))))
        dp (magnitude
            (vector-minus
             (vector-plus line-from (scalar-vector-multiplication ct line))
             circle-position))
        d (- dp circle-radius)]
    d))

(defn get-line-direction [from to]
  (map - to from))

(defn get-shape-coll-polygon-points [shape-coll-polygon]
  (mapcat (fn [{:keys [from to]}] [from to]) (:shapes shape-coll-polygon)))

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
  (let [points (get-shape-coll-polygon-points shape-coll-polygon)]
    [(get-points-bounding-box points)]))

(defn get-bounding-boxes [{:keys [shape type shapes] :as object}]
  (cond
    (= shape :circle) (get-circle-bounding-boxes object)
    (= shape :line) (get-line-bounding-boxes object)
    (and (= shape :shape-coll) (= type "shape-coll-polygon"))
      (get-shape-coll-polygon-bounding-boxes object)
    (= shape :shape-coll) (mapcat get-bounding-boxes shapes)))

(defn bounding-boxes-overlapping? [
  {min-x-a :min-x max-x-a :max-x min-y-a :min-y max-y-a :max-y}
  {min-x-b :min-x max-x-b :max-x min-y-b :min-y max-y-b :max-y}]
  (not
    (or
      (< max-x-a min-x-b)
      (> min-x-a max-x-b)
      (< max-y-a min-y-b)
      (> min-y-a max-y-b))))

(defn shapes-overlapping? [o1 o2]
  (let [bounding-boxes1 (get-bounding-boxes o1)
  bounding-boxes2 (get-bounding-boxes o2)]
  (some
    (fn [bounding-box1]
      (some (partial bounding-boxes-overlapping? bounding-box1) bounding-boxes2))
    bounding-boxes1))
  )

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

(defn get-y-at-x [pick-y x points]
  (reduce
   (fn [max-y [other-x y]]
     (if (and (= other-x x) (or (not max-y) (pick-y y max-y)))
       y
       max-y))
   nil
   points))

(def get-max-y-at-x (partial get-y-at-x >))
(def get-min-y-at-x (partial get-y-at-x <))