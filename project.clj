(defproject avoid "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.json "0.2.6"]
                 [quil "2.7.1"]
                ;  [uncomplicate/neanderthal "0.20.4"]
                 ]
  :main avoid.core
  :aliases {"avoid" ["run" "/home/christian/avoid/src/avoid/games/avoid-falling.json"]
  "copter" ["run" "/home/christian/avoid/src/avoid/games/copter.json"]})
