(ns clj.day11-test
  (:require [clojure.test :refer :all]
            [clj.day11 :refer :all]))

(deftest cell-fuel-level-test
  (testing "cell fuel level calculation"
    (is (= 4 (cell-fuel-level 8 [3 5])))))

(deftest make-grid-test
  (testing "grid creation"
    (is (= -5 (get-in (make-grid 57) [79 122])))
    (is (= 0 (get-in (make-grid 39) [196 217])))
    (is (= 4 (get-in (make-grid 71) [153 101])))))

(deftest calculate-total-power-of-test
  (testing "power of calculation"
    (is (= 29 (calculate-total-power-of (make-grid 18) [33 45])))))