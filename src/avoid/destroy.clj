(ns avoid.destroy)

(defn create-fn [pred]
  (fn [game-size input-key other-objects object]
    (if
     (pred {:game-size game-size
            :input-key input-key
            :other-objects other-objects
            :object object})
      object)))

(defmacro create [& body]
  `(create-fn
    (fn [{:keys [~'game-size ~'input-key ~'other-objects ~'object]}]
      (let [{:keys [~'direction ~'position ~'radius ~'color ~'from ~'to ~'shapes]} ~'object
            [~'game-width ~'game-height] ~'game-size
            [~'x ~'y] ~'position]
        (if (not (do ~@body)) ~'object)))))

