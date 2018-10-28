(ns avoid.object)

(defn create [& object-fields]
  (let [object (apply (partial merge {:id (gensym) :color [255 255 255]}) object-fields)]
    (assert (every? (partial contains? object) [:id :color :position :direction :radius :update-fns]))
    object))