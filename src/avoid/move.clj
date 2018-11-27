(ns avoid.move
  (:require [avoid.util :as util]))

(defn move-circle [step {:keys [position direction] :as object} add-to-position]
  (assoc
   object
   :position
   (util/vector-plus
    position
    add-to-position
    (util/scalar-vector-multiplication step direction))))

(defn move-line [step {:keys [from to direction] :as line} add-to-position]
  (let [step-direction
    (util/vector-plus add-to-position (util/scalar-vector-multiplication step direction))]
    (merge
     line
     {:from (util/vector-plus from step-direction)
      :to (util/vector-plus to step-direction)})))

(defn move
  ([step object] (move step object [0.0 0.0]))
  ([step {:keys [shape] :as object} add-to-position]
   (cond
     (= shape :circle) (move-circle step object add-to-position)
     (= shape :line) (move-line step object add-to-position)
     (= shape :shape-coll)
     (let
      [d (:direction object)
       shape-coll-step-direction (util/scalar-vector-multiplication step (:direction object))
       next-add-to-position (util/vector-plus shape-coll-step-direction add-to-position)]
       (assoc object :shapes
              (map
               (fn [o] (move step o next-add-to-position))
               (:shapes object))))
     :else object)))

(defn move-objects [game-size step objects]
  (map (partial move step) objects))
