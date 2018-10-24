(ns avoid.engine
  (:require [avoid.util :refer :all]
            [avoid.update :as update]
            [avoid.destroy :as destroy]
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

(defn get-collision-object
  ([objects object] (get-collision-object objects object 0.01))
  ([objects object collision-distance]
   (some
    (fn [other-object]
      (let [t (min-pos-collision-time object other-object) d (distance object other-object)]
        (and
         (< (distance object other-object) collision-distance)
       ; (collision? object other-object)
      ; t
      ;  (<= t 0.0)
         other-object)))
    (without-object objects object))))

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


(defn update-object [game-size input-key other-objects object]
  (reduce
    (fn [current-object update-fn]
      (let [next-object (update-fn game-size input-key other-objects current-object)]
        (if (some? next-object) next-object (reduced nil))))
    object
    (:update-fns object)))

(defn update-objects-sequential [game-size input-key objects]
  (let
   [updated-objects
    (reduce
     (fn [new-objects object]
       (let [other-objects (concat new-objects (drop (inc (count new-objects)) objects))
             updated-object (update-object game-size input-key other-objects object)]
         (if (some? updated-object)
           (cons updated-object new-objects)
           new-objects)))
     []
     objects)]
    (vec updated-objects)))

(defn update-objects [game-size input-key objects]
  (->>
    objects
    (map (fn [object] (update-object game-size input-key (without-object objects object) object)))
    (filter some?)
    vec))

(defn update-tick2
  ([game-size input-key objects] (update-tick2 game-size input-key objects 1.0))
  ([game-size input-key objects time-left] (update-tick2 game-size input-key objects time-left 0.0025))
  ([game-size input-key objects time-left min-tick]
   (if (< time-left min-tick)
     objects
     (let
      [earliest-collision-time (find-earliest-collision-time objects)
       earliest-edge-bounce (find-earliest-edge-collision game-size objects)
       time-step (apply (partial min time-left) (filter (every-pred some? pos?) [earliest-collision-time earliest-edge-bounce]))
       moved-objects (move-objects game-size time-step objects)
       updated-objects (update-objects game-size input-key moved-objects)]
       (update-tick2 game-size nil updated-objects (- time-left time-step))))))

(def destroy-on-exit-bottom
  (destroy/create-destroy
   (let [[x y] position] (<= y radius))))

(def destroy-on-collision
  (destroy/create-destroy (get-collision-object other-objects object 0.1)))

(defn hpa [direction action]
  (cond
    (= action :up) (vector-plus [0 0.2] direction)
    (= action :down) (vector-plus [0 -0.2] direction)
    (= action :left) (vector-plus [-0.2 0] direction)
    (= action :right) (vector-plus [0.2 0] direction)
    :else direction))

(def handle-player-action (update/create-update :direction (hpa direction input-key)))
(def decrease-direction (update/create-update :direction (scalar-vector-multiplication 0.99 direction)))
(def bounce-edges-update (update/create-update :direction (bounce-edges game position direction radius)))
(def color-by-position (update/create-update :color (get-color-by-vector position)))
(def take-color-on-collision
  (update/create-update
   :color
   (let [co (get-collision-object other-objects object 0.1)]
     (if co (:color co) color))))

(def collision-counter
  (update/create-update
   :collisions
   (if (get-collision-object other-objects object 0.1)
     (do (println "prev collisions" collisions) (inc collisions))
     collisions)))

(defn create-object [& object-fields]
  (apply (partial merge {:id (gensym)}) object-fields))

(defn create-player [color]
  (create-object
   {:position [100 100] :direction [1 1] :radius 20 :color color
    :collisions 0
    :update-fns [handle-player-action
                 decrease-direction
                 bounce-edges-update
                 take-color-on-collision
                 collision-counter]}))

(defn create-circle [position radius]
  (create-object
   {:position position :radius radius :direction [0 -1] :color [255 100 100]
    :update-fns [color-by-position destroy-on-exit-bottom destroy-on-collision]}))

(defn find-opening-for-circle
  ([[width height] radius objects] (find-opening-for-circle [width height] radius objects 0))
  ([[width height] radius objects attempts]
   (let [position
         [(+ radius (rand-int (- width (* 2 radius)))) (+ radius (rand-int (- height (* 2 radius))))]]
     (if (not (overlapping-any? objects {:position position :radius radius :id (gensym)}))
       position
       (if (< attempts 10)
         (find-opening-for-circle [width height] radius objects (inc attempts)))))))

(defn add-random-circle [[width height] objects]
  (if (>= (count objects) 10)
    objects
    (let [radius 10 color [255 100 255]
          position (find-opening-for-circle [width height] radius objects)]
      (if (some? position)
        (cons (create-circle position radius) objects)
        objects))))

(defn update-tick [game-size input-key objects]
  (->> (update-tick2 game-size input-key objects) (add-random-circle game-size)))

(defn setup [[width height]]
  (add-random-circle [width height] [(create-player [255 255 100])]))