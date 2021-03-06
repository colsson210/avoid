(ns avoid.core
  (:require [avoid.quiladapter :as quiladapter]
            [avoid.tick :as tick]
            [avoid.settings :as settings]
            [avoid.object :as object]
            [avoid.game :as game]))

(defn handle-add-objects-coll [add-objects-coll key state]
  (reduce
   (fn [acc-state {:keys [add-objects-template add-objects-fn add-objects-preds]}]
     (let [a-ok
           (every?
            (fn [{:keys [add-objects-pred add-objects-pred-args]}]
              ((apply partial add-objects-pred add-objects-pred-args) acc-state key))
            add-objects-preds)]
       (if a-ok
         (cons
          (add-objects-fn settings/game-size add-objects-template acc-state)
          acc-state)
         acc-state)))
   state
   add-objects-coll))

(defn update-state [add-objects-coll state key]
  (settings/key-input-handler key)
  (->>
   (tick/tick settings/game-size key state)
   (handle-add-objects-coll add-objects-coll key)))

(defn game-tick [lose? win? update-state state key]
  (cond
    (lose? state key)
    (do
      (println "Lose!")
      (quiladapter/exit))
    (win? state key)
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
     (do
       (quiladapter/start
        initial-state
        (partial game-tick lose? win? (partial update-state add-objects)))))))