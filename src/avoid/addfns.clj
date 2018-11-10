(ns avoid.addfns
  (:require
   [avoid.util :as util]
   [avoid.object :as object]))

(defn add-random-circle [[game-width game-height] template objects]
  (let [radius 10
        position (util/find-opening-for-circle [[0 game-width] [0 game-height]] radius objects)]
    (if (some? position)
      (object/create template {:position position :radius radius}))))

(comment
  (defn add-random-circle-right [[game-width game-height] template objects]
    (if ())))

(defn add-roof [[game-width game-height] template objects]
  (let [roofs (filter (fn [o] (= (:type o) "roof")) objects)
        roofs-points-xs (map first (mapcat :points roofs))
        max-roof-x (if (seq? roofs-points-xs) (apply max roofs-points-xs) 0)]
    (object/create
     template
     {:color [100 100 100] :points (map (fn [[x y]] [(+ x max-roof-x) y]) (:points template))})))