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
   (let [position
         [(+ x-min radius (rand-int (- x-max (+ x-min (* 2 radius)))))
          (+ y-min radius (rand-int (- y-max (+ y-min (* 2 radius)))))]]
     (if (not (overlapping-any? objects {:position position :radius radius :id (gensym)}))
       position
       (if (< attempts 10)
         (find-opening-for-circle [[x-min x-max] [y-min y-max]] radius objects (inc attempts)))))))

(def pos-number? (comp some? pos?))

(defn point-within [[width height] [x y]]
  (and (>= x 0) (<= x width) (>= y 0) (<= y height)))

(defn line-points-within [size [from to]]
  (and
   (point-within size from)
   (point-within size to)))

(defn get-cave-segments [state]
  (filter (comp (partial = "cave") :type) state))

(defn get-cave-points [state]
  (let [cave-segments (get-cave-segments state)
        shapes (mapcat :shapes cave-segments)
        points (mapcat :points shapes)]
    points))

(defn get-cave-shapes [state]
  (let [cave-segments (get-cave-segments state)
        shapes (mapcat :shapes cave-segments)]
    shapes))

(defn get-cave-shapes-at-x [x objects]
  (let [shapes (get-cave-shapes objects)]
    (filter
     (fn [{:keys [points]}] (some (comp (partial = x) first) points))
     shapes)))

(defn get-cave-max-x [state]
  (let [cave-segments (get-cave-segments state)
        shapes (mapcat :shapes cave-segments)
        points (mapcat :points shapes)
        xs (map first points)]
    (if (empty? xs) 0 (apply max xs))))