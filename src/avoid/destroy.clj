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
    (fn [{:keys [~'game ~'input-key ~'other-objects ~'object]}]
      (let [{:keys [~'direction ~'position ~'radius ~'color]} ~'object]
        (if (not (do ~@body)) ~'object)))))

