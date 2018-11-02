(ns avoid.core
  (:require [avoid.quiladapter :as quiladapter]
            [avoid.tick :as tick]
            [avoid.settings :as settings]
            [avoid.object :as object]
            [avoid.game :as game]))

(defn handle-add-objects [add-objects add-objects-pred state]
  (if (add-objects-pred state)
    (let [object-to-add (add-objects state)]
      (if object-to-add (cons object-to-add state) state))
    state))

(defn handle-add-objects-coll [add-objects-coll state]
  (reduce
   (fn [acc-state {:keys [add-objects-fn add-objects-pred add-objects-pred-args add-objects-template]}]
     (if ((partial apply add-objects-pred add-objects-pred-args) acc-state)
       (cons
        (add-objects-fn settings/game-size add-objects-template acc-state)
        acc-state)
       acc-state))
   state
   add-objects-coll))

(defn update-state [remove-objects add-objects add-objects-pred state key]
  (let [key (quiladapter/get-key-input)]
    (settings/key-input-handler key)
    (->>
     (tick/tick settings/game-size key state)
     remove-objects
     (handle-add-objects add-objects add-objects-pred))))

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
         {:keys [add-objects player-initial-state player lose-condition-fn lose-condition-fn-args]} game
         initial-state [(object/create player-initial-state player)]
         remove-objects identity
         {:keys [add-objects-fn add-objects-pred add-objects-template add-objects-pred-args]} add-objects
         add-objects-fn2 (if add-objects-fn (partial add-objects-fn settings/game-size add-objects-template) identity)
         add-objects-pred2 (if add-objects-pred (apply partial add-objects-pred add-objects-pred-args) (constantly false))
         lose? (if lose-condition-fn (apply partial lose-condition-fn lose-condition-fn-args) (constantly false))
         win? (constantly false)]
     (println game)
     (quiladapter/start
      initial-state
      (partial game-tick lose? win? (partial update-state remove-objects add-objects-fn2 add-objects-pred2))))))