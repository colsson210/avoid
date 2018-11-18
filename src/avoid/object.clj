(ns avoid.object)

(defn create [& object-fields]
  (let [object (apply (partial merge {:id (gensym) :color [255 255 255]}) (filter some? object-fields))]
    (if (= (:shape object) :shape-coll)
      (assert (contains? object :shapes))
      (assert (every? (partial contains? object) [:id :color :shape :direction :update-fns]) (str object)))
    (cond
      (= (:shape object) :circle)
      (assert (every? (partial contains? object) [:position :radius]))
      (= (:shape object) :line)
      (assert (every? (partial contains? object) [:from :to])))
    (if (= (:shape object) :shape-coll)
      (update
        object
        :shapes
        (partial map create))
    object
    )
    ))