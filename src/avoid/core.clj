(ns avoid.core
  (:require [avoid.quiladapter :as quiladapter]
            [avoid.tick :as tick]
            [avoid.settings :as settings]
            [avoid.object :as object]
            [avoid.game :as game]))

(defn update-state [remove-objects add-objects state key]
  (let [key (quiladapter/get-key-input)]
    (settings/key-input-handler key)
    (->>
     (tick/tick settings/game-size remove-objects add-objects key state)
     remove-objects
     add-objects)))

(defn game-tick [lose? win? update-state state key]
  (cond
    (lose? state)
    (do
      (println "Lose!")
      (quiladapter/exit))
    (win? state)
    (do
      (println "Win!" state)
      (quiladapter/exit))
    :else (update-state state key)))

(defn -main
  ([] (println "Missing argument: game.json"))
  ([game-json]
   (let [game (game/read-template game-json)
         initial-state [(object/create (:player-initial-state game) (:player game))]
         remove-objects identity
         add-objects (partial object/add-random-circle settings/game-size (:add-objects-template game))
         lose? (apply partial (:lose-condition-fn game) (:lose-condition-fn-args game))
         win? (constantly false)]
        (println game)
     (quiladapter/start
      initial-state
      (partial game-tick lose? win? (partial update-state remove-objects add-objects))))))