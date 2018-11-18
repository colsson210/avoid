(ns avoid.core
  (:require [avoid.quiladapter :as quiladapter]
            [avoid.tick :as tick]
            [avoid.settings :as settings]
            [avoid.object :as object]
            [avoid.game :as game]))

(defn handle-add-objects-coll [add-objects-coll state]
  (reduce
   (fn [acc-state {:keys [add-objects-fn add-objects-pred add-objects-pred-args add-objects-template]}]
     (if ((apply partial add-objects-pred add-objects-pred-args) acc-state)
       (cons
        (add-objects-fn settings/game-size add-objects-template acc-state)
        acc-state)
       acc-state))
   state
   add-objects-coll))

(defn update-state [add-objects-coll state key]
  (settings/key-input-handler key)
  (->>
   (tick/tick settings/game-size key state)
   (handle-add-objects-coll add-objects-coll)))

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
         {:keys [add-objects lose-condition-fn lose-condition-fn-args objects-initial-state]} game
         lose? (if lose-condition-fn (apply partial lose-condition-fn lose-condition-fn-args) (constantly false))
         win? (constantly false)
         initial-state (map object/create objects-initial-state)]
     (println initial-state)
    (if true
      (do
        (quiladapter/start
          initial-state
         (partial game-tick lose? win? (partial update-state add-objects)))
        ))
      )))