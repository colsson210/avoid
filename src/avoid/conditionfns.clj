(ns avoid.conditionfns)

(defn collisions-above [limit state]
  (some->> state (some :collisions) (<= limit)))