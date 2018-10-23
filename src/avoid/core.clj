(ns avoid.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [avoid.engine :as engine]
            [avoid.settings :as settings]))

(defn setup []
  (q/frame-rate @settings/frame-rate)
  (q/color-mode :hsb)
  (engine/setup [settings/window-width settings/window-height]))

(defn get-key-input []
  (if (q/key-pressed?) (q/key-as-keyword)))

(defn update-state [state]
  (let [key (get-key-input)]
    (settings/key-input-handler key)
    (engine/update-tick [settings/window-width settings/window-height] key state)))

(defn draw-circle [[x y] radius color]
  (let [draw-radius (* 2 radius)]
    (q/stroke-weight 1)
    (q/with-translation [0 0])
    (apply q/fill color)
    (q/ellipse x (- settings/window-height y) draw-radius draw-radius)))

(defn draw-line [[x y] [dx dy]]
  (q/stroke-weight 3)
  (q/with-translation [0 0])
  (q/line x (- settings/window-height y) (+ x (* 20 dx)) (- settings/window-height (+ y (* 20 dy)))))

(defn clear-sketch [] (q/background 240))

(defn draw-state [state]
  (do
    (clear-sketch)
    (q/frame-rate @settings/frame-rate)
    (dorun
     (for [{:keys [position radius color]} state]
       (draw-circle position radius color)))))


(defn -main [& args]
  (q/defsketch avoid
    :title "Avoid!!! :)"
    :size [settings/window-width settings/window-height]
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))

