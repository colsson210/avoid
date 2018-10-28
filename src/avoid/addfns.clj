(ns avoid.addfns
  (:require
   [avoid.util :as util]
   [avoid.object :as object]))

(defn add-random-circle [game-size template objects]
  (if (>= (count objects) 10)
    objects
    (let [radius 10
          position (util/find-opening-for-circle game-size radius objects)]
      (if (some? position)
        (cons
         (object/create template {:position position :radius radius})
         objects)
        objects))))