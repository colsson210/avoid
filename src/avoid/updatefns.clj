(ns avoid.updatefns
  (:require [avoid.util :as util]
            [avoid.update :as update]
            [avoid.collision :as collision]))

(defn get-circle-direction-after-circle-collision [{other-direction :direction other-position :position} {:keys [position direction]}]
  (:v1-next
   (util/collide-circles
    position
    direction
    other-position
    other-direction)))

(defn a-b-c [from to position]
  (let [c (util/vector-minus position from)
        line (util/vector-minus to from)
        normalized-line (util/normalize line)
        a (util/scalar-vector-multiplication
           (util/dot-product c normalized-line)
           normalized-line)
        b (util/vector-minus c a)]
    {:a a :b b :c c}))

(defn get-circle-direction-after-line-collision [{:keys [from to]} {:keys [position direction radius]}]
  (let [line (util/vector-minus to from)
        direction-magnitude (util/magnitude direction)
        normalized-line (util/normalize line)
        from-to-circle (util/vector-minus position from) ; c
        direction-on-line (util/scalar-vector-multiplication (util/dot-product direction normalized-line) normalized-line)
        from-to-circle-on-line (util/scalar-vector-multiplication (util/dot-product from-to-circle normalized-line) normalized-line) ; a
        collision-normal (util/vector-minus from-to-circle from-to-circle-on-line) ; b
        tangential-direction (util/scalar-vector-multiplication (util/dot-product direction normalized-line) line)]
    (util/scalar-vector-multiplication
     direction-magnitude
     (util/normalize
      (util/vector-plus collision-normal direction-on-line)))))

(defn get-line-direction-after-line-collision [collision-line {:keys [direction] :as line}]
  (util/scalar-vector-multiplication -1 direction))

(defn get-direction-after-collision [objects {:keys [shape id direction] :as object}]
  (let
   [{collision-shape :shape :as collision-object} (collision/get-collision-object objects object)]
    (if (some? collision-object) (println "collision-object" collision-object))
    (if (some? collision-object)
      (cond
        (and (= shape :circle) (= collision-shape :circle)) (get-circle-direction-after-circle-collision collision-object object)
        (and (= shape :circle) (= collision-shape :line)) (get-circle-direction-after-line-collision collision-object object)
        (and (= shape :line) (= collision-shape :line))
        (get-line-direction-after-line-collision collision-object object)
        :else (util/scalar-vector-multiplication -1 direction))
      direction)))

(defn gravity
  ([direction] (gravity direction 0.01))
  ([[x y] y-decrease]
   [x (max -10.0 (- y y-decrease))]))

(defn get-color-by-vector [[x y]]
  [(mod x 256) (mod y 256) 255])

(defn hpa [direction action]
  (let [step 1.0]
    (cond
      (= action :up) (util/vector-plus [0 step] direction)
      (= action :down) (util/vector-plus [0 (- step)] direction)
      (= action :left) (util/vector-plus [(- step) 0] direction)
      (= action :right) (util/vector-plus [step 0] direction)
      :else direction)))

(def bounce-objects (update/create :direction (get-direction-after-collision other-objects object)))
(def handle-player-action (update/create :direction (hpa direction input-key)))
(def copter-handle-player-input
  (update/create :direction (if (= input-key :up) (util/vector-plus [0 0.4] direction) direction)))
(def decrease-direction (update/create :direction (util/scalar-vector-multiplication 0.99 direction)))
(def color-by-position (update/create :color (get-color-by-vector position)))
(def take-color-on-collision
  (update/create
   :color
   (let [co (collision/get-collision-object other-objects object)]
     (if co (:color co) color))))
(def collision-counter
  (update/create
   :collisions
   (if (collision/get-collision-object other-objects object)
     (do (println "prev collisions" collisions) (inc collisions))
     collisions)))
(def gravity-update (update/create :direction (gravity direction)))

(def copter-gravity
  (update/create :direction (gravity direction 0.02)))