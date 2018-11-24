(ns avoid.addfns
  (:require
   [avoid.util :as util]
   [avoid.object :as object]))

(defn create-random-circle [[game-width game-height] template objects]
  (let [radius (get template :radius 10)
        position (util/find-opening-for-circle [[radius (- game-width radius)] [radius (- game-height radius)]] radius objects)]
    (if (some? position)
      (object/create template {:shape :circle :position position :radius radius}))))

(defn create-cave-segment [[game-width game-height] template start-x start-y-lower start-y-upper]
  (let [cave-segment-template (:cave-segment-template template)
        segment-width (:segment-width template)
        segment-height (:segment-height template)
        end-x (+ start-x segment-width)
      start-y-lower-tenth (int (/ start-y-lower 10))
        end-y-lower (min (- game-height segment-height) (+ start-y-lower (- start-y-lower-tenth) (rand-int (* 2 start-y-lower-tenth))))
        end-y-upper (+ end-y-lower segment-height)
        points-upper [[start-x game-height] [start-x start-y-upper] [end-x (max 0 (min game-height end-y-upper))] [end-x 500]]
        points-lower [[start-x 0] [start-x start-y-lower] [end-x (max 0 end-y-lower)] [end-x 0]]]
    (object/create
     (assoc template :shape :polygon)
     {:shapes [(object/create cave-segment-template {:points points-lower :shape :polygon})
               (object/create cave-segment-template {:points points-upper :shape :polygon})]})))

(defn create-cave-segment-addon [template objects]
  (if (not-any? (fn [{:keys [type]}] (= type "cave")) objects)
    {:start-x 0
     :start-y-lower 0
     :start-y-upper (+ (:segment-height template))}
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
; (println "add-cave-segment" objects)
  (let [{:keys [start-x start-y-lower start-y-upper]}
        (create-cave-segment-addon template objects)]
    (create-cave-segment game-size template start-x start-y-lower start-y-upper)))

