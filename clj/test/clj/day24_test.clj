(ns clj.day24-test
  (:require [clojure.test :refer :all]
            [clj.day24 :refer :all]))

(deftest select-targets-test
  (testing "Selects correct targets"
    (reset! id 0)
    (let [immune (parse-groups :immune "day24-example-immune.txt")
          infection (parse-groups :infection "day24-example-infection.txt")]
      (is (= {3 1, 1 4, 4 2, 2 3} (select-targets (concat immune infection)))))))