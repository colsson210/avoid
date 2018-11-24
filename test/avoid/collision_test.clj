(ns avoid.collision-test
  (:require [clojure.test :refer :all]
            [avoid.collision :as collision]))



(deftest collision-time-line-circle-test
  (testing "collision-time-line-circle"
    (let [circle-position [0 0]
          circle-radius 1
          circle-direction [0 0]
          line-from [1 0]
          line-to [1 1]
          line-direction [0 0]
          collision-time (collision/collision-time-line-circle
                          {:from line-from :to line-to :direction line-direction}
                          {:position circle-position :radius circle-radius :direction circle-direction})]
      (is (= collision-time 0)))))

(deftest collision-line-line?-test
  (testing "collision-line-line?"
    (is (= (collision/collision-line-line? [0 0] [1 1] [1 0] [0 1]) true))))

(deftest collision-time-line-line-test
  (testing "collision-time-line-line"
    (is
     (=
      (collision/collision-time-line-line
       {:direction [0 0] :from [0 0] :to [1 1]}
       {:direction [0 0] :from [1 0] :to [0 1]})
      0))))


      