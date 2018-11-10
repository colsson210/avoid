(ns avoid.conditionfns)

(defn collisions-above [limit state]
  (some->> state (some :collisions) (<= limit)))

(defn objects-below [limit state]
  (< (count state) limit))

(defn no-player [state]
  (not-any? (comp (partial = "player") :type) state))

(defn roof-below-width [width state]
  (let [roofs (filter (comp (partial = "roof") :type) state)]
    (if (some? roofs)
      (let [max-roof-x (apply max (mapcat (fn [r] (map first (:points r))) roofs))]
        (< max-roof-x width)))))