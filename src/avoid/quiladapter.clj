(ns avoid.quiladapter
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [avoid.settings :as settings]
            [avoid.object :as object]))

(defn exit [] (q/exit))

(defn get-key-input []
  (if (q/key-pressed?) (q/key-as-keyword)))

(defn draw-circle [[x y] radius color]
  (let [draw-radius (* 2 radius)]
    (q/stroke-weight 1)
    (q/with-translation [0 0])
    (apply q/fill color)
    (q/ellipse x (- settings/window-height y) draw-radius draw-radius)))

(defn draw-line [[x0 y0] [x1 y1]]
  (q/stroke-weight 3)
  (q/with-translation [0 0])
  (q/line x0 (- settings/window-height y0) x1 (- settings/window-height y1)))

(defn draw-polygon [points]
  (q/fill 100 255 100)
  (q/begin-shape)
  (dorun
   (for [[x y] points]
     (q/vertex x (- settings/window-height y))))
  (q/end-shape :close))

(defn draw-polygon-lines [lines]
  (let [points (mapcat (fn [{:keys [from to]}] [from to]) lines)]
    (dorun (draw-polygon points))))

(defn draw-shape [{:keys [position radius color shape from to points shapes type]}]
  (dorun
    (cond
      (= shape :circle) (draw-circle position radius color)
      (= shape :line) (draw-line from to)
      (and (= shape :shape-coll) (= type "shape-coll-polygon"))
        (draw-polygon-lines shapes)
      (= shape :shape-coll)
       (dorun (for [s shapes] (draw-shape s))))))

(defn clear-sketch [] (q/background 240))

(defn draw-state [state]
  (do
    (clear-sketch)
    (dorun
     (for [shape state]
       (dorun (draw-shape shape))))))

(defn setup [initial-state]
  (q/frame-rate @settings/frame-rate)
  (q/color-mode :hsb)
  initial-state)

(defn get-update-fn [update-state]
  (fn [state] (update-state state (get-key-input))))

(defn start [initial-state update-state]
  (q/defsketch
    avoid
    :title "Avoid!!! :)"
    :size [settings/window-width settings/window-height]
    :setup (partial setup initial-state)
    :update (get-update-fn update-state)
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
