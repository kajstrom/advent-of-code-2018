(ns clj.day16-test
  (:require [clojure.test :refer :all]
            [clj.day16 :refer :all]))

(deftest test-opcodes
  (testing "addr"
    (is (= [2 2 4 0] (addr [2 2 0 0] 0 1 2))))
  (testing "addi"
    (is (= [2 4 0 0] (addi [2 0 0 0] 0 2 1))))
  (testing "mulr"
    (is (= [2 3 6 0] (mulr [2 3 0 0] 0 1 2))))
  (testing "muli"
    (is (= [2 6 0 0] (muli [2 0 0 0] 0 3 1))))
  (testing "banr"
    (is (= [3 5 1 0] (banr [3 5 0 0] 0 1 2))))
  (testing "bani"
    (is (= [3 1 0 0] (bani [3 0 0 0] 0 5 1))))
  (testing "borr"
    (is (= [3 5 7 0] (borr [3 5 0 0] 0 1 2))))
  (testing "bori"
    (is (= [3 7 0 0] (bori [3 0 0 0] 0 5 1))))
  (testing "setr"
    (is (= [5 5 0 0] (setr [5 0 0 0] 0 0 1))))
  (testing "seti"
    (is (= [5 0 0 0] (seti [0 0 0 0] 5 0 0))))
  (testing "gtir"
    (is (= [3 1 0 0] (gtir [3 0 0 0] 4 0 1)))
    (is (= [3 0 0 0] (gtir [3 0 0 0] 3 0 1)))
    )
  (testing "gtri"
    (is (= [3 1 0 0] (gtri [3 0 0 0] 0 4 1)))
    (is (= [3 0 0 0] (gtri [3 0 0 0] 0 3 1)))
    )
  (testing "gtrr"
    (is (= [4 3 1 0] (gtrr [4 3 0 0] 0 1 2)))
    (is (= [3 4 0 0] (gtrr [3 4 0 0] 0 1 2)))
    )
  (testing "eqir"
    (is (= [5 1 0 0] (eqir [5 0 0 0] 5 0 1)))
    (is (= [5 0 0 0] (eqir [5 0 0 0] 6 0 1)))
    )
  (testing "eqri"
    (is (= [5 1 0 0] (eqri [5 0 0 0] 0 5 1)))
    (is (= [5 0 0 0] (eqri [5 0 0 0] 0 6 1)))
    )
  (testing "eqrr"
    (is (= [5 5 1 0] (eqrr [5 5 0 0] 0 1 2)))
    (is (= [5 6 0 0] (eqrr [5 6 0 0] 0 1 2)))
    )
  )
