(ns avoid.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [avoid.engine :as engine]
            [avoid.settings :as settings]))

(defn multiply-direction [object]
  (assoc object :direction (map (partial * 0.99) (:direction object))))

(defn add-direction [direction object]
  (assoc object :direction (map + (:direction object) direction)))

(defn handle-player-action [player action]
  (cond
    (= action :up) (add-direction [0 0.2] player)
    (= action :down) (add-direction [0 -0.2] player)
    (= action :left) (add-direction [-0.2 0] player)
    (= action :right) (add-direction [0.2 0] player)
    :else player))

(defn setup []
  (q/frame-rate @settings/frame-rate)
  (q/color-mode :hsb)
  [{:position [100 100] :direction [1 1] :radius 20
    :update [(fn [game-size other-objects player] (multiply-direction player))
             (engine/create-update-fn :direction (fn [{:keys [game object]}]
                                                   (engine/bounce-edges game (:position object) (:direction object) (:radius object))))]
    :on-object-collision (fn [object other-object] (println "lose") object)
    :on-edge-collision nil
    :key-handler handle-player-action
    :color [255 255 200]}])

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

