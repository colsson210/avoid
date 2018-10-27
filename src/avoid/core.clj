(ns avoid.core
  (:require [avoid.quiladapter :as quiladapter]
            [avoid.tick :as tick]
            [avoid.settings :as settings]
            [avoid.object :as object]
            [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]))

(def initial-state
  (object/add-random-circle
   [settings/window-width settings/window-height]
   [(object/create-player)]))

(defn update-state [state key]
  (let [key (quiladapter/get-key-input)]
    (settings/key-input-handler key)
    (tick/tick [settings/window-width settings/window-height] key state)))

(defn -main [& args]
  (quiladapter/start initial-state update-state))