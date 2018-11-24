(ns avoid.util-test
  (:require [clojure.test :refer :all]
            [avoid.util :as util]))

(deftest distance-line-circle-test
  (testing "distance line circle"
    (let [circle-position [0 0]
          circle-radius 1
          line-from [1 0]
          line-to [1 1]
          distance (util/distance-line-circle circle-position circle-radius line-from line-to)]
      (is (= distance 0.0)))))