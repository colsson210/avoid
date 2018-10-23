(ns avoid.update)

(defn create-update-fn [field func]
  (fn [game input-key other-objects object]
    (assoc
     object
     field
     (func {:game game :input-key input-key :other-objects other-objects :object object}))))

(defmacro create-update [update-key & body]
  `(create-update-fn
    ~update-key
    (fn [{:keys [~'game ~'input-key ~'other-objects ~'object]}] (do ~@body))))


