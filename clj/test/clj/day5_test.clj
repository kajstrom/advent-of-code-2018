(ns clj.day5-test
  (:require [clojure.test :refer :all]
            [clj.day5 :refer :all]))

(deftest react-all-test
  (testing "reacting all polymers"
    (is (= "dabCBAcaDA" (react-all "dabAcCaCBAcCcaDA")))
    (is (= "dabCBAcaD" (react-all "dabCBAcaDAa")))
    (is (= "aabAAB" (react-all "aabAAB")))
    (is (= "abAB" (react-all "abAB")))
    (is (= "" (react-all "abBA")))
    (is (= "Mt" (react-all "qLlZzGtToOJsSjvhHVJjgQbeEBMthH")))
    (is (= "Ugy" (react-all "qLkKleEZHhTtzGtiITowWOJjlLJkKZzRrQqsvVSjvzZrRhTtHVJvVjAavVRrgQMmAabegGKkEBnNMthVvoOHTkKjJCcmnNvVSlLstToxXYcfFUuCNnyOpPoOUgOoZwWzzxXZhpPxXHGgeExXnNzEeZMmy")))
    (is (= "DbLEBhY" (react-all "ZziIDbDdKkfUugGFLlLEBIitTzrRrRZCctjwWJThVvkKVwWvY")))
    ))
