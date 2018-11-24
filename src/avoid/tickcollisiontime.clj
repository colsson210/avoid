(comment
  (ns avoid.tickcollisiontime
    (:require [avoid.util :as util]
              [avoid.collisiontime :as collision]))
  (defn tick-collision-time
    ([game-size input-key objects] (tick-collision-time game-size input-key objects 1.0))
    ([game-size input-key objects time-left] (tick-collision-time game-size input-key objects time-left 0.0025))
    ([game-size input-key objects time-left min-tick]
     (if (< time-left min-tick)
       objects
       (let
        [earliest-collision-time (collisiontime/find-earliest-collision-time objects)
         time-step (if (some? earliest-collision-time) (min time-left earliest-collision-time) time-left)
         moved-objects (move-objects game-size time-step objects)
         updated-objects (update-objects game-size input-key moved-objects)]
         (tick-collision-time game-size nil updated-objects (- time-left time-step))))))
  )