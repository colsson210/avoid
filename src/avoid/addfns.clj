(ns avoid.addfns
  (:require
   [avoid.util :as util]
   [avoid.object :as object]))

(defn add-random-circle [[game-width game-height] template objects]
  (if (>= (count objects) 10)
    objects
    (let [radius 10
          position (util/find-opening-for-circle [[0 game-width] [0 game-height]] radius objects)]
      (if (some? position)
        (cons
         (object/create template {:position position :radius radius})
         objects)
        objects))))

(comment
  (defn add-random-circle-right [[game-width game-height] template objects]
    (if ()))
  )