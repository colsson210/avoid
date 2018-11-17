(ns avoid.addfns
  (:require
   [avoid.util :as util]
   [avoid.object :as object]))

(defn add-random-circle [[game-width game-height] template objects]
  (let [radius 10
        position (util/find-opening-for-circle [[0 game-width] [0 game-height]] radius objects)]
    (if (some? position)
      (object/create template {:position position :radius radius}))))

(defn create-cave-segment [template start-x start-y-lower start-y-upper]
  (let [cave-segment-template (:cave-segment-template template)
        segment-width (:segment-width template)
        segment-height (:segment-height template)
        end-x (+ start-x segment-width)
        end-y-lower (min (- 500 segment-height) (+ start-y-lower -100 (rand-int 200)))
        end-y-upper (+ end-y-lower segment-height)
        points-upper [[start-x 500] [start-x start-y-upper] [end-x (min 499 end-y-upper)] [end-x 500]]
        points-lower [[start-x 0] [start-x start-y-lower] [end-x (max 1 end-y-lower)] [end-x 0]]]
    (object/create
     template
     {:shapes [(object/create cave-segment-template {:points points-lower})
               (object/create cave-segment-template {:points points-upper})]})))

(defn create-cave-segment-addon [template objects]
  (if (not-any? (fn [{:keys [type]}] (= type "cave")) objects)
    {:start-x 0
     :start-y-lower 100
     :start-y-upper 400}
    (let [max-x (util/get-cave-max-x objects)
          cave-shapes-at-x (util/get-cave-shapes-at-x max-x objects)
          start-y-lower (reduce
                         (fn [min-max-y val]
                           (let
                            [y (util/get-max-y-at-x max-x (:points val))]
                             (if (or (not y) (and y (< y min-max-y))) y min-max-y)))
                         (util/get-max-y-at-x max-x (:points (first cave-shapes-at-x)))
                         cave-shapes-at-x)
          cave-shapes-at-x-without-yl (filter (fn [cs] (not= (util/get-max-y-at-x max-x cs) start-y-lower)) cave-shapes-at-x)
          start-y-upper (reduce
                         (fn [curr val]
                           (let [y (util/get-min-y-at-x max-x (:points val))]
                             (if (and y (> y curr) (> y start-y-lower)) y curr)))
                         (util/get-min-y-at-x max-x (:points (first cave-shapes-at-x-without-yl)))
                         cave-shapes-at-x-without-yl)]
      {:start-x max-x
       :start-y-lower start-y-lower
       :start-y-upper start-y-upper})))

(defn add-cave-segment [game-size template objects]
  (let [{:keys [start-x start-y-lower start-y-upper]}
        (create-cave-segment-addon template objects)]
    (create-cave-segment template start-x start-y-lower start-y-upper)))

