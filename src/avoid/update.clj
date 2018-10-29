(ns avoid.update)

(defn create-fn [field func]
  (fn [game-size input-key other-objects object]
    (assoc
     object
     field
     (func {:game-size game-size :input-key input-key :other-objects other-objects :object object}))))

(defmacro create [update-key & body]
  `(create-fn
    ~update-key
    (fn [{:keys [~'game-size ~'input-key ~'other-objects ~'object]}]
      (let [{:keys [~'direction ~'position ~'radius ~'color ~'collisions]} ~'object
            [~'game-width ~'game-height] ~'game-size
            [~'x ~'y] ~'position]
        (do ~@body)))))

