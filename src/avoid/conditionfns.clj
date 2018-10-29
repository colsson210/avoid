(ns avoid.conditionfns)

(defn collisions-above [limit state]
  (some->> state (some :collisions) (<= limit)))

(defn no-player [state]
  (not-any? (comp (partial = "player") :type) state))