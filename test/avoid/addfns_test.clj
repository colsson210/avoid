(ns avoid.addfns-test
  (:require [clojure.test :refer :all]
            [avoid.addfns :as addfns]))

(deftest create-random-circle-test
  (testing "create a circle"
    (let [game-size [100 100]
          template {}
          objects []
          circle (addfns/create-random-circle game-size template objects)]
      (is (some? circle))))
  (testing "Don't create a circle"
    (let [game-size [100 100]
          template {:radius 100}
          objects []
          circle (addfns/create-random-circle game-size template objects)]
      (is (= circle nil)))))

(deftest create-cave-segment-test
  (testing "create a cave segment"
    (let
     [template {:cave-segment-element-template {} :segment-width 10 :segment-height 10}
      start-x 0
      start-y-lower 0
      start-y-upper 100
      game-size [1000 1000]
      cave-segment (addfns/create-cave-segment game-size template start-x start-y-lower start-y-upper)]
      (is (= (count (:shapes cave-segment)) 2)))))

(deftest create-cave-segment-addon-test
  (testing "create a cave segment addon when there are no other cave segments"
    (let
     [template {:segment-height 10}
      cave-segment-addon (addfns/create-cave-segment-addon template [])]
      (is (= cave-segment-addon {:start-x 0, :start-y-lower 0, :start-y-upper 10})))))

(deftest add-cave-segment-test
  (testing "add a cave segment"
    (let [template {:cave-segment-element-template {} :segment-width 10 :segment-height 10}
          game-size [100 100]
          objects []
          cave-segment (addfns/add-cave-segment game-size template objects)]
      (is (some? cave-segment)))))