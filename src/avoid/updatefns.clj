(ns avoid.updatefns
  (:require [avoid.util :as util]
            [avoid.update :as update]
            [avoid.collision :as collision]))

(defn get-direction-after-collision [objects {:keys [position direction] :as object}]
  (let
   [{c-position :position c-direction :direction} (collision/get-collision-object objects object)]
    (if c-position
      (:v1-next (util/collide
                 position
                 direction
                 c-position
                 c-direction))
      direction)))

(defn gravity [[x y]]
  [x (max -10.0 (- y 0.1))])

(defn get-color-by-vector [[x y]]
  [(mod x 256) (mod y 256) 255])

(defn bounce-edges [[width height] [px py] [dx dy] radius]
  (let [max-x (- width radius) max-y (- height radius)
        min-x radius min-y radius]
    [(if (or (<= px min-x) (>= px max-x))
       (* -1 dx)
       dx)
     (if (or (<= py min-y) (>= py max-y))
       (* -1 dy)
       dy)]))

(defn hpa [direction action]
  (cond
    (= action :up) (util/vector-plus [0 0.2] direction)
    (= action :down) (util/vector-plus [0 -0.2] direction)
    (= action :left) (util/vector-plus [-0.2 0] direction)
    (= action :right) (util/vector-plus [0.2 0] direction)
    :else direction))

(def handle-player-action (update/create :direction (hpa direction input-key)))
(def decrease-direction (update/create :direction (util/scalar-vector-multiplication 0.99 direction)))
(def bounce-edges-update (update/create :direction (bounce-edges game position direction radius)))
(def color-by-position (update/create :color (get-color-by-vector position)))
(def take-color-on-collision
  (update/create
   :color
   (let [co (collision/get-collision-object other-objects object 0.1)]
     (if co (:color co) color))))
(def collision-counter
  (update/create
   :collisions
   (if (collision/get-collision-object other-objects object 0.1)
     (do (println "prev collisions" collisions) (inc collisions))
     collisions)))
(def gravity-update (update/create :direction (gravity direction)))