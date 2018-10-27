(ns avoid.update)

(defn create-fn [field func]
  (fn [game input-key other-objects object]
    (assoc
     object
     field
     (func {:game game :input-key input-key :other-objects other-objects :object object}))))

(defmacro create [update-key & body]
  `(create-fn
    ~update-key
    (fn [{:keys [~'game ~'input-key ~'other-objects ~'object]}]
      (let [{:keys [~'direction ~'position ~'radius ~'color ~'collisions]} ~'object]
        (do ~@body)))))