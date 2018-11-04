(ns avoid.object)

(defn create [& object-fields]
  (let [object (apply (partial merge {:id (gensym) :color [255 255 255]}) object-fields)]
    (assert (every? (partial contains? object) [:id :color :shape :direction :update-fns]))
    (cond
      (= (:shape object) :circle)
      (assert (every? (partial contains? object) [:position :radius]))
      (= (:shape object) :line)
      (assert (every? (partial contains? object) [:from :to])))
    object))