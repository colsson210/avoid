(ns avoid.tick
  (:require [avoid.util :as util]
            [avoid.collision :as collision]))

(defn ensure-circle-within-bounds [[width height] {:keys [radius position] :as circle}]
  (let [[x y] position]
    (assoc
     circle
     :position
     (map (partial max radius) [(min x (- width radius)) (min y (- height radius))]))))

(defn move [[width height] step {:keys [radius position direction] :as object}]
  (let
   [[x1 y1] (util/vector-plus
             position
             (util/scalar-vector-multiplication step direction))]
    (assoc
     object
     :position
     (map (partial max radius) [(min x1 (- width radius)) (min y1 (- height radius))]))))

(defn move-objects [game-size step objects]
  (reduce
   (fn [moved-objects object]
     (let [moved-object (move game-size step object)]
       (conj moved-objects (if (util/overlapping-any? (concat moved-objects (nthrest objects (count moved-objects))) moved-object) object moved-object))))
   []
   objects))

(defn update-object [game-size input-key other-objects object]
  (reduce
   (fn [current-object update-fn]
     (println object)
     (println update-fn)
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
   (map
    (fn [object] (update-object game-size input-key (util/without-object objects object) object)))
   (filter some?)
   vec))

(defn tick-step
  ([game-size input-key objects] (tick-step game-size input-key objects 1.0))
  ([game-size input-key objects time-left] (tick-step game-size input-key objects time-left 0.0025))
  ([game-size input-key objects time-left min-tick]
   (if (< time-left min-tick)
     objects
     (let
      [earliest-collision-time (collision/find-earliest-collision-time objects)
       earliest-edge-bounce (collision/find-earliest-edge-collision game-size objects)
       time-step (apply (partial min time-left) (filter (every-pred some? pos?) [earliest-collision-time earliest-edge-bounce]))
       moved-objects (move-objects game-size time-step objects)
       updated-objects (update-objects game-size input-key moved-objects)]
       (tick-step game-size nil updated-objects (- time-left time-step))))))

(defn tick [game-size input-key objects] (tick-step game-size input-key objects))
