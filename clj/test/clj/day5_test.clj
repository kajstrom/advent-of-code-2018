(ns clj.day5-test
  (:require [clojure.test :refer :all]
            [clj.day5 :refer :all]))

(deftest react?-test
  (testing "chars react"
    (is (react? \c \C))
    (is (react? \C \c)))
  (testing "chars don't react"
    (is (= false (react? \C \C)))
    (is (= false (react? \c \c)))
    (is (= false (react? \b \C)))))

(deftest react-polymers-test
  (testing "polymer reacting"
    (is (= "dabAaCBAcaDA" (react-polymers "dabAcCaCBAcCcaDA")))
    (is (= "dabCBAcaDA" (react-polymers "dabAaCBAcaDA")))
    (is (= "dabCBAcaDA" (react-polymers "dabCBAcaDA")))
    (is (= "aA" (react-polymers "abBA")))
    (is (= "" (react-polymers "aA")))
    (is (= "" (react-polymers "aAaA")))
    (is (= "abAB" (react-polymers "abAB")))
    )
  )

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
