(ns clj.day9-test
  (:require [clojure.test :refer :all]
            [clj.day9 :refer :all]))

(deftest next-idx-test
  (testing "finds next index"
    (is (= 1 (next-idx [0] 0)))
    (is (= 1 (next-idx [0 1] 1)))
    (is (= 3 (next-idx [0 1 2] 1)))
    (is (= 1 (next-idx [0 2 1 3] 3)))
    (is (= 3 (next-idx [0 4 2 1 3] 1)))))

(deftest place-marble-test
  (testing "places marble correctly"
    (is (= [0 1] (place-marble [0] 1 1)))
    (is (= [0 2 1] (place-marble [0 1] 1 2)))
    (is (= [0 2 1 3] (place-marble [0 2 1] 3 3)))
    (is (= [0 4 2 1 3] (place-marble [0 2 1 3] 1 4)))))

(deftest marble-idx-to-remove-test
  (testing "finds idx to remove"
    (is (= 3 (marble-idx-to-remove [0 1 2 3 4 5 6] 3)))))

(deftest remove-marble-test
  (testing "removes marble"
    (is (= [0 1 2 4 5 6] (remove-marble [0 1 2 3 4 5 6] 3)))))