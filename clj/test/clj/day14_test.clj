(ns clj.day14-test
  (:require [clojure.test :refer :all]
            [clj.day14 :refer :all]))

(deftest create-new-recipes-test
  (testing "recipe creation"
    (is (= [1 0] (create-new-recipes [(make-elf 0) (make-elf 1)] [3 7])))
    (is (= [1] (create-new-recipes [(make-elf 3) (make-elf 4)] [3 7 1 0 1 0])))))

(deftest move-elves-test
  (testing "elf moving"
    (let [elves [(make-elf 0) (make-elf 1)]]
      (move-elves elves [3 7])
      (is (= 0 (:pos (first elves))))
      (is (= 1 (:pos (last elves)))))
    (let [elves [(make-elf 4) (make-elf 3)]]
      (move-elves elves [3 7 1 0 1 0 1])
      (is (= 4 (:pos (last elves))))
      (is (= 6 (:pos (first elves)))))
    (let [elves [(make-elf 1) (make-elf 2)]]
      (move-elves elves [0 1 0 0])
      (is (= 3 (:pos (first elves))))
      (is (= 0 (:pos (last elves)))))))
