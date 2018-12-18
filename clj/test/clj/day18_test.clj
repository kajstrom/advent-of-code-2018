(ns clj.day18-test
  (:require [clojure.test :refer :all]
            [clj.day18 :refer :all]))

(deftest open-next-test
  (testing "changes to trees if three or more adjacent cells contain trees"
    (is (= :trees (open-next [:trees :trees :trees])))
    (is (= :trees (open-next [:trees :trees :trees :trees :open :open :lumberyard])))
    )
  (testing "remains open if not adjacent to three trees"
    (is (= :open (open-next [:open :open :lumberyard])))))

(deftest trees-next-test
  (testing "becomes a lumberyard if three or more cells contain lumberyards"
    (is (= :lumberyard (trees-next [:lumberyard :lumberyard :lumberyard :open :trees :trees])))
    )
  (testing "remains as trees if less than three adjacent lumberyards"
    (is (= :trees (trees-next [:lumberyard :lumberyard :trees :trees :open])))))

(deftest lumberyard-next-test
  (testing "remains as a lumberyard if adjacent has atleast one lumberyard and one tree"
    (is (= :lumberyard (lumberyard-next [:lumberyard :trees :open :open]))))
  (testing "becomes open if not "
    (is (= :open (lumberyard-next [:open :open :lumberyard])))))