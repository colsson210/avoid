(ns avoid.tick
  (:require [avoid.util :as util]
    [avoid.move :as move]
            [avoid.collision :as collision]))

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
   (map
    (fn [object] (update-object game-size input-key (util/without-object objects object) object)))
   (filter some?)
   vec))

(defn use-old-positions [old-objects new-objects]
  (map
   (fn [{:keys [shape position from to shapes] :as old-object} {new-shapes :shapes :as new-object}]
     (cond
       (= shape :circle) (assoc new-object :position position)
       (= shape :line) (merge new-object {:from from :to to})
       (= shape :shape-coll) (assoc new-object :shapes 
        (use-old-positions shapes new-shapes))))
   old-objects
   new-objects))

(defn tick-old-positions-on-collision
  ([game-size input-key objects] (tick-old-positions-on-collision game-size input-key objects 1.0))
  ([game-size input-key objects time-left] (tick-old-positions-on-collision game-size input-key objects time-left 0.1))
  ([game-size input-key objects time-left tick-length]
   (if (< time-left tick-length)
     objects
     (let
      [moved-objects (move/move-objects game-size tick-length objects)
       updated-objects (update-objects game-size input-key moved-objects)
        next-objects (if
         (not (collision/some-collision? updated-objects))
         updated-objects
         (use-old-positions objects updated-objects))
        ]
       (tick-old-positions-on-collision game-size nil next-objects (- time-left tick-length) tick-length)))))

(defn tick [game-size input-key objects]
  (let [tick-fn tick-old-positions-on-collision]
    (tick-fn game-size input-key objects)))