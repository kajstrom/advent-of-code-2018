(ns clj.day15-test
  (:require [clojure.test :refer :all]
            [clj.day15 :refer :all]))

(reset! logging-enabled false)

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

(deftest filter-equal-or-less-cells-test
  (testing "filters already visited cells"
    (is (empty? (filter-equal-or-less-cells [[4 2 1]] [[4 2 0]])))
    (is (= [[4 2 1]] (filter-equal-or-less-cells [[4 2 1]] [[4 5 1]])))
    (is (= [[4 3 1]] (filter-equal-or-less-cells [[4 3 1] [4 2 0]] [[4 5 1] [4 2 0]])))
    ))

(deftest play-until-victory-test
  (testing "score calculation"
    (reset! elf-attack-power 3)
    (let [[cavern-map units] (parse-map-and-units "day15-example6.txt")]
      (is (= 27730 (play-until-victory report-victory units cavern-map))))
    (let [[cavern-map units] (parse-map-and-units "day15-example7.txt")]
      (is (= 36334 (play-until-victory report-victory units cavern-map))))
    (let [[cavern-map units] (parse-map-and-units "day15-example13.txt")]
      (is (= 39514 (play-until-victory report-victory units cavern-map))))
    (let [[cavern-map units] (parse-map-and-units "day15-example8.txt")]
      (is (= 27755 (play-until-victory report-victory units cavern-map))))
    (let [[cavern-map units] (parse-map-and-units "day15-example9.txt")]
      (is (= 28944 (play-until-victory report-victory units cavern-map))))
    (let [[cavern-map units] (parse-map-and-units "day15-example10.txt")]
      (is (= 18740 (play-until-victory report-victory units cavern-map))))
    (let [[cavern-map units] (parse-map-and-units "day15-example12.txt")]
      (is (= 13987 (play-until-victory report-victory units cavern-map))))
    ))

(deftest choose-next-move-destination-test-2
  (testing "pathfinding"
    (let [[cavern-map units] (parse-map-and-units "day15-example14.txt")]
      (is (= [1 3] (choose-next-move-destination (first units) units cavern-map)))
      (is (= [2 3] (choose-next-move-destination (last units) units cavern-map)))
      )
    (let [[cavern-map units] (parse-map-and-units "day15-example15.txt")]
      (is (= [2 2] (choose-next-move-destination (first units) units cavern-map)))
      (is (= [2 4] (choose-next-move-destination (last units) units cavern-map)))
      )
    (let [[cavern-map units] (parse-map-and-units "day15-example16.txt")]
      (is (= [2 3] (choose-next-move-destination (first units) units cavern-map)))
      )
    ))

(deftest path-length-test
  (testing "path length calculation"
    (is (= 3 (path-length [[2 2 0] [1 2 1] [2 3 1] [3 2 1] [1 1 2] [1 3 2] [2 4 3]])))

    ))