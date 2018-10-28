(ns avoid.core
  (:require [avoid.quiladapter :as quiladapter]
            [avoid.tick :as tick]
            [avoid.settings :as settings]
            [avoid.object :as object]
            [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.game :as game]))

(defn update-state [remove-objects add-objects state key]
  (let [key (quiladapter/get-key-input)]
    (settings/key-input-handler key)
    (tick/tick settings/game-size remove-objects add-objects key state)))

(defn -main
  ([] (println "Missing game json argument"))
  ([game-json]
   (let [game (game/read-template game-json)
         initial-state [(object/create (:player-initial-state game) (:player game))]
         remove-objects identity
         add-objects (partial object/add-random-circle settings/game-size (:add-objects-template game))]
     (quiladapter/start initial-state (partial update-state remove-objects add-objects)))))