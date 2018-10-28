(ns avoid.settings)

(def window-width 500)
(def window-height 500)
(def game-size [500 500])
(def max-frame-rate 200)
(def min-frame-rate 2)
(def frame-rate (atom 120))

(defn key-input-handler [key]
  (cond
    (= key :+)
    (swap! frame-rate (comp (partial min max-frame-rate) inc))
    (= key :-)
    (swap! frame-rate (comp (partial max min-frame-rate) dec))))