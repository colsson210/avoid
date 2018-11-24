(ns avoid.move
  (:require [avoid.util :as util]))

(defn move-circle [step {:keys [position direction] :as object}]
  (assoc
   object
   :position
   (util/vector-plus
    position
    (util/scalar-vector-multiplication step direction))))

(defn move-line [step {:keys [from to direction] :as line}]
  (let [step-direction (util/scalar-vector-multiplication step direction)]
    (merge
     line
     {:from (util/vector-plus from step-direction)
      :to (util/vector-plus to step-direction)})))

(defn move-polygon [step {:keys [points direction] :as polygon}]
  (let [step-direction (util/scalar-vector-multiplication step direction)]
    (assoc
     polygon
     :points
     (map (partial util/vector-plus step-direction) points))))

(defn move [step {:keys [shape] :as object}]
  (cond
    (= shape :circle) (move-circle step object)
    (= shape :line) (move-line step object)
    (= shape :polygon) (move-polygon step object)
    (= shape :shape-coll)
    (let
     [d (:direction object)
      shape-coll-step-direction (util/scalar-vector-multiplication step (:direction object))]
      (assoc object :shapes
             (map
              (partial move step)
              (:shapes object))))
    :else object))

(defn move-objects [game-size step objects]
  (reduce
   (fn [moved-objects object]
     (let [moved-object (move step object)]
       (conj moved-objects (if (util/overlapping-any? (concat moved-objects (nthrest objects (count moved-objects))) moved-object) object moved-object))))
   []
   objects))