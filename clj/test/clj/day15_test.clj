(ns clj.day15-test
  (:require [clojure.test :refer :all]
            [clj.day15 :refer :all]))

(deftest choose-next-move-destination-test
  (testing "finds available path"
    (let [[cavern-map units] (parse-map-and-units "day15-example2.txt")]
      (is (= [1 2] (choose-next-move-destination (first units) units cavern-map)))
      (is (= [1 3] (choose-next-move-destination (second units) units cavern-map)))
      (is (= [2 2] (choose-next-move-destination (nth units 2) units cavern-map)))
      (is (= nil (choose-next-move-destination (nth units 3) units cavern-map))))))

(deftest enemy-to-attack-test
  (testing "finds enemy to attack"
    (let [[cavern-map units] (parse-map-and-units "day15-example4.txt")
          enemy (enemy-to-attack (first units) cavern-map)]
      (is (and (= 2 (:x enemy)) (= 1 (:y enemy))))))
  (testing "returns nil when no enemy to attack"
    (let [[cavern-map units] (parse-map-and-units "day15-example2.txt")]
      (is (nil? (enemy-to-attack (first units) cavern-map))))))
