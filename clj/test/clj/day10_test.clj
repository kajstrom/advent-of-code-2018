(ns clj.day10-test
  (:require [clojure.test :refer :all]
            [clj.day10 :refer :all]))

(deftest move-point-test
  (testing "moves point"
    (let [point (transient {:pos [3 9] :vel [1, -2]})]
      (is (= [4, 7] (:pos (move-point point))))
      (is (= [5, 5] (:pos (move-point point))))
      (is (= [6, 3] (:pos (move-point point))))
      )))
