(ns avoid.conditionfns
  (:require
    [avoid.util :as util]))

(defn collisions-above [limit state]
  (some->> state (some :collisions) (<= limit)))

(defn objects-below [limit state]
  (< (count state) limit))

(defn no-player [state]
  (not-any? (comp (partial = "player") :type) state))

(defn cave-segment-below-width [width state]
  (< (util/get-cave-max-x state) width))