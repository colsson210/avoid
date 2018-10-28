(ns avoid.object
  (:require [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.util :as util]
            [clojure.data.json :as json]))

(defn resolve-symbol [x]
  ((comp (partial ns-resolve 'avoid.object) symbol) x))

(defn read-template [json-filename]
  (json/read-str
   (slurp json-filename)
   :key-fn keyword
   :value-fn (fn [key value] (if (= key :update-fns) (map resolve-symbol value) value))))

(defn create [& object-fields]
  (let [object (apply (partial merge {:id (gensym) :color [255 255 255]}) object-fields)]
    (assert (every? (partial contains? object) [:id :color :position :direction :radius :update-fns]))
    object))

(defn add-random-circle [game-size template objects]
  (if (>= (count objects) 10)
    objects
    (let [radius 10
          position (util/find-opening-for-circle game-size radius objects)]
      (if (some? position)
        (cons
         (create template {:position position :radius radius})
         objects)
        objects))))