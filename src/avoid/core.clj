(ns avoid.core
  (:require [avoid.quiladapter :as quiladapter]
            [avoid.tick :as tick]
            [avoid.settings :as settings]
            [avoid.object :as object]
            [avoid.updatefns :as updatefns]
            [avoid.destroyfns :as destroyfns]
            [avoid.game :as game]))

(defn update-state [state key]
  (let [key (quiladapter/get-key-input)
        add-objects (partial object/add-random-circle settings/game-size)
        remove-objects identity]
    (settings/key-input-handler key)
    (tick/tick settings/game-size remove-objects add-objects key state)))

(defn -main [& args]
  (let [
    game-json "/home/christian/avoid/src/avoid/games/avoid-falling.json"
    game (game/read-template game-json)
    initial-state [(object/create (:player-initial-state game) (:player game))]
  ]
  (quiladapter/start initial-state update-state)))