(ns avoid.updatefns-test
  (:require [clojure.test :refer :all]
            [avoid.updatefns :refer :all]))

(deftest a-b-c-test
  (testing "a-b-c test1"
    (let [from [0 0] to [2 0] position [1 1] {:keys [a b c]} (a-b-c from to position)]
      (is (= c position))
      (is (= a [1.0 0.0]))
      (is (= b [0.0 1.0])))))


    (comment
      
      (deftest get-circle-direction-after-line-collision-test
        (testing "test1 abc123"
          (let [from [0 0] to [2 0] position [1 1] direction [0 -1] radius 1]
            (is
             (=
              (get-circle-direction-after-line-collision
               {:from from :to to}
               {:position position :direction direction :radius radius})
              [0 1])))))

      
      )      
