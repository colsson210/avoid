(ns avoid.engine
  (:require [avoid.util :refer :all]
            [clojure.math.combinatorics :as combinatorics]))

(defn gravity [[x y]]
  [x (max -10.0 (- y 0.1))])

(defn min-pos-collision-time [{a-position :position a-direction :direction a-radius :radius} {b-position :position b-direction :direction b-radius :radius}]
  (let [c-ts (collision-times
              a-position a-direction a-radius
              b-position b-direction b-radius)]
    (if (some? c-ts)
      (let [positive-collision-times (filter pos? c-ts)]
        (if (not (empty? positive-collision-times))
          (apply min positive-collision-times))))))

(defn overlapping-any? [other-objects {:keys [id position radius]}]
  (some?
   (some (fn [{other-id :id other-position :position other-radius :radius}]
           (and (not= id other-id)
                (< (position-distance position other-position) (+ radius other-radius))))
         other-objects)))

(defn get-color-by-vector [[x y]]
  [(mod x 256) (mod y 256) 255])

(defn create-update-fn [field func]
  (fn [game other-objects object] (assoc object field (func {:game game :other-objects other-objects :object object}))))

(defn without-object [objects {:keys [id]}]
  (filter (comp (partial not= id) :id) objects))

(defn distance [& circles]
  (let [sides-length (apply + (apply (partial map (comp square -)) (map :position circles)))
        h-length (apply (comp square +) (map :radius circles))]
    (- sides-length h-length)))

(defn collision? [& circles]
  (let [sides-length (apply + (apply (partial map (comp square -)) (map :position circles)))
        h-length (apply (comp square +) (map :radius circles))]
    (<= (- sides-length h-length) 0.0)))

(defn bounce-edges [[width height] [px py] [dx dy] radius]
  (let [max-x (- width radius) max-y (- height radius)
        min-x radius min-y radius]
    [(if (or (<= px min-x) (>= px max-x))
       (* -1 dx)
       dx)
     (if (or (<= py min-y) (>= py max-y))
       (* -1 dy)
       dy)]))

(defn get-collision-object [objects object]
  (some
   (fn [other-object]
     (let [t (min-pos-collision-time object other-object) d (distance object other-object)]
       (and
        (< (distance object other-object) 0.01)
       ; (collision? object other-object)
      ; t
      ;  (<= t 0.0)
        other-object)))
   (without-object objects object)))

(defn get-direction-after-collision [objects {:keys [position direction] :as object}]
  (let
   [{c-position :position c-direction :direction} (get-collision-object objects object)]
    (if c-position
      (:v1-next (collide
                 position
                 direction
                 c-position
                 c-direction))
      direction)))

(defn get-circle-object
  ([position direction radius] (get-circle-object position direction radius [100 255 255]))
  ([position direction radius color]
   {:id (gensym)
    :position position
    :direction direction
    :radius radius
    :color color
    :update
    [(create-update-fn :color (comp get-color-by-vector :position :object))
     (create-update-fn :direction (fn [{:keys [other-objects object]}] (get-direction-after-collision other-objects object)))
     (create-update-fn :direction (fn [{:keys [game object]}]
                                    (bounce-edges game (:position object) (:direction object) (:radius object))))
     (create-update-fn :direction (comp gravity :direction :object))]
    :destruction-predicates [(constantly false)]}))

(defn survive-update? [{:keys [:destruction-predicates] :as object}]
  (not ((apply some-fn destruction-predicates) object)))

(defn find-earliest-collision-time [objects]
  (reduce
   (fn [earliest [a b]]
     (let [t (min-pos-collision-time a b)]
       (if (or (not earliest) (and t (< t earliest))) t earliest)))
   nil
   (combinatorics/combinations objects 2)))

(defn edge-collision-time [[width height] {[px py] :position [vx vy] :direction r :radius}]
  (let
   [tx (if (> (Math/abs vx) 0)
         [(/ (- r px) vx) (/ (- (- width px) r) vx)])
    ty (if (> (Math/abs vy) 0)
         [(/ (- r py) vy) (/ (- (- height py) r) vy)])
    possible-times (filter (fn [t] (and t (> t 0))) (concat tx ty))]
    (if (> (count possible-times) 0)
      (apply min possible-times))))

(defn find-earliest-edge-collision [game-size objects]
  (let [edge-collision-times (filter (fn [t] (and t (> t 0))) (map (partial edge-collision-time game-size) objects))]
    (if (> (count edge-collision-times) 0) (apply min edge-collision-times))))

(defn move [[width height] step {:keys [radius position direction] :as object}]
  (let
   [[x1 y1] (vector-plus
             position
             (scalar-vector-multiplication step direction))]
    (assoc
     object
     :position
     (map (partial max radius) [(min x1 (- width radius)) (min y1 (- height radius))]))))

(defn move-objects [game-size step objects]
  (reduce
   (fn [moved-objects object]
     (let [moved-object (move game-size step object)]
       (conj moved-objects (if (overlapping-any? (concat moved-objects (nthrest objects (count moved-objects))) moved-object) object moved-object))))
   []
   objects))

(defn update-objects [game-size objects]
  (let
   [updated-objects
    (reduce
     (fn [new-objects object]
       (let [other-objects (concat new-objects (drop (inc (count new-objects)) objects))
             updated-object (reduce
                             (fn [o f] (f game-size other-objects o))
                             object
                             (:update object))]
         (cons updated-object new-objects)))
     []
     objects)]
    (vec updated-objects)))

(defn update-tick2
  ([game-size objects] (update-tick2 game-size objects 1.0))
  ([game-size objects time-left] (update-tick2 game-size objects time-left 0.0025))
  ([game-size objects time-left min-tick]
   (if (< time-left min-tick)
     objects
     (let
      [earliest-collision-time (find-earliest-collision-time objects)
       earliest-edge-bounce (find-earliest-edge-collision game-size objects)
       time-step (apply (partial min time-left) (filter (every-pred some? pos?) [earliest-collision-time earliest-edge-bounce]))
       moved-objects (move-objects game-size time-step objects)
       updated-objects (update-objects game-size moved-objects)]
       (update-tick2 game-size updated-objects (- time-left time-step))))))

(defn update-tick [game-size key objects]
  (update-tick2 game-size (map (fn [object] ((:key-handler object) object key)) objects)))

(defn create-random-circle [[width height] objects]
  (let [; radius (+ 10 (rand-int 20))
        radius 20
        new-circle (get-circle-object
                    [(max radius (rand-int (- width radius))) (max radius (rand-int (- height radius)))]
                    ; [(+ -1 (rand 2)) (+ -1 (rand 2))]
                    [1.0 1.0]
                    radius)]
    (if (overlapping-any? objects new-circle)
      (create-random-circle [width height] objects)
      new-circle)))