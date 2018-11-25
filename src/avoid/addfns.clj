(ns avoid.addfns
  (:require
   [avoid.util :as util]
   [avoid.object :as object]))

(defn create-random-circle [[game-width game-height] template objects]
  (let [radius (get template :radius 10)
        position (util/find-opening-for-circle [[radius (- game-width radius)] [radius (- game-height radius)]] radius objects)]
    (if (some? position)
      (object/create template {:shape :circle :position position :radius radius}))))

(defn create-cave-segment-element-lines [points]
  (let [point-pairs
        (filter
         some?
         (map
          (fn [from to] (let [[fx fy] from [tx ty] to]
                          (if (not (and (= fx tx) (= fy ty)))
                            [from to])))
          points
          (concat (rest points) (list (first points)))))]
    (map
     (fn [[from to]]
       (object/create {:shape :line :from from :to to :direction [-5 0.0]}))
     point-pairs)))

(defn create-cave-segment-element [cave-segment-element-template points]
  (let [double-points (map (fn [[x y]] [(double x) (double y)]) points)]
    (object/create
     cave-segment-element-template
     {:shapes (create-cave-segment-element-lines double-points)})))

(defn create-cave-segment [[game-width game-height] template start-x start-y-lower start-y-upper]
  (let [cave-segment-element-template (:cave-segment-element-template template)
        segment-width (:segment-width template)
        segment-height (:segment-height template)
        end-x (+ start-x segment-width)
        start-y-lower-tenth (int (/ start-y-lower 4))
        end-y-lower (min (- game-height segment-height) (+ start-y-lower (- start-y-lower-tenth) (rand-int (* 2 start-y-lower-tenth))))
        end-y-upper (+ end-y-lower segment-height)
        points-upper [[start-x game-height] [start-x start-y-upper] [end-x (max 0 (min game-height end-y-upper))] [end-x 500]]
        points-lower [[start-x 0] [start-x start-y-lower] [end-x (max 0 end-y-lower)] [end-x 0]]]
      ; (println "points-lower" points-lower)
    (object/create
     (dissoc template :cave-segment-element-template)
     {:shapes
      [(create-cave-segment-element cave-segment-element-template points-lower)
       (create-cave-segment-element cave-segment-element-template points-upper)
       ]})))

(defn get-cave-segment-addon-positions [[game-width game-height] template objects]
  (if (not-any? (fn [{:keys [type]}] (= type "cave-segment")) objects)
    (let [segment-height (:segment-height template)
    start-y-lower (/ (- game-height segment-height) 2)]
      {:start-x 0
       :start-y-lower start-y-lower
       :start-y-upper (+ start-y-lower segment-height)}
      )
    (let [max-x (util/get-cave-segment-max-x objects)
          cave-segment-elements (util/get-cave-segment-elements-at-x max-x objects)
          start-y-lower (reduce
                         (fn [min-max-y-at-x cave-segment-element]
                           (let [cave-segment-element-points (util/get-cave-segment-element-points cave-segment-element)
                                 max-y-at-x (util/get-max-y-at-x max-x cave-segment-element-points)]
                             (if
                              (or (not min-max-y-at-x) (and max-y-at-x (< max-y-at-x min-max-y-at-x)))
                                  max-y-at-x
                                  min-max-y-at-x)))
                         nil
                         cave-segment-elements)
          start-y-upper (reduce
                         (fn [max-min-y-at-x cave-segment-element]
                           (let [cave-segment-element-points (util/get-cave-segment-element-points cave-segment-element)
                                 min-y-at-x (util/get-min-y-at-x max-x cave-segment-element-points)]
                             (if
                              (or (not max-min-y-at-x) (and min-y-at-x (> min-y-at-x max-min-y-at-x)))
                               min-y-at-x
                               max-min-y-at-x)))
                         nil
                         cave-segment-elements)]
      {:start-x max-x
       :start-y-lower start-y-lower
       :start-y-upper start-y-upper})))

(defn add-cave-segment [game-size template objects]
  (let [{:keys [start-x start-y-lower start-y-upper]}
        (get-cave-segment-addon-positions game-size template objects)]
    (create-cave-segment game-size template start-x start-y-lower start-y-upper)))

