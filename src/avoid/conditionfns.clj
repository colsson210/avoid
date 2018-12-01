(ns avoid.conditionfns
  (:require
   [avoid.util :as util]
   [avoid.cavesegment :as cavesegment]))

(defn collisions-above [limit state key-pressed]
  (some->> state (some :collisions) (<= limit)))

(defn objects-below [limit state key-pressed]
  (< (count state) limit))

(defn no-player [state key-pressed]
  (not-any? (comp (partial = "player") :type) state))

(defn cave-segment-below-width [width state key-pressed]
  (< (cavesegment/get-cave-segment-max-x state) width))

(defn object-type-count-below-limit [type limit state key-pressed]
  (->>
   state
   (filter (comp (partial = type) :type))
   count
   (> limit)))

(defn cave-segment-max-x-above [limit state key-pressed]
  (> (cavesegment/get-cave-segment-max-x state) limit))

(defn player-pressed [key state key-pressed]
  (= (keyword key) key-pressed))