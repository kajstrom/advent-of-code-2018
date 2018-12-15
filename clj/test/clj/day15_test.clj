(ns clj.day15-test
  (:require [clojure.test :refer :all]
            [clj.day15 :refer :all]))

(deftest choose-next-move-destination-test
  (testing "finds available path"
    (let [[cavern-map units] (parse-map-and-units "day15-example2.txt")]
      (is (= [1 2] (choose-next-move-destination (first units) units cavern-map)))
      (is (= [1 3] (choose-next-move-destination (second units) units cavern-map)))
      (is (= [2 2] (choose-next-move-destination (nth units 2) units cavern-map)))
      (is (= nil (choose-next-move-destination (nth units 3) units cavern-map)))
      )))
