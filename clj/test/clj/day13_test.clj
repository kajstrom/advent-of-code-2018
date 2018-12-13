(ns clj.day13-test
  (:require [clojure.test :refer :all]
            [clj.day13 :refer :all]))

(deftest parse-track-row-test
  (testing "track parsing"
    (is (= '(:curve-r :straight :straight :straight :straight :curve-l) (parse-track-row "/----\\")))
    (is (= '(:straight nil nil :curve-r :straight :straight :intersection :straight :straight :curve-l) (parse-track-row "|  /--+--\\")))
    (is (= '(:straight :straight :straight :straight) (parse-track-row "^><v")))))

(deftest parse-carts-test
  (testing "cart parsing"
    (is (= 2 (count (parse-carts (track-from-file "day13-example.txt")))))))

(deftest turn-cart-intersection-test
  (testing "cart turning at intersection"
    (is (= :west (:direction (turn-cart-intersection (make-cart 0 0 :north)))))
    (is (= :east (:direction (turn-cart-intersection (make-cart 0 0 :south)))))
    (is (= :north (:direction (turn-cart-intersection (make-cart 0 0 :east)))))
    (is (= :south (:direction (turn-cart-intersection (make-cart 0 0 :west))))))
  (testing "cart turning at intersection sequence"
    (let [cart (make-cart 0 0 :north)]
      (is (= (:west (:direction (turn-cart-intersection cart)))))
      (is (= (:west (:direction (turn-cart-intersection cart)))))
      (is (= (:north (:direction (turn-cart-intersection cart)))))
      )
    )
  (testing "cart turning at intersection changes :next-turn-idx"
    (let [cart (make-cart 0 0 :north)]
      (is (= 0 (:next-turn-idx cart)))
      (is (= 1 (:next-turn-idx (turn-cart-intersection cart))))
      (is (= 2 (:next-turn-idx (turn-cart-intersection cart))))
      (is (= 0 (:next-turn-idx (turn-cart-intersection cart))))
      ))
  )

(deftest turn-cart-curve-test
  (testing "cart turning at an intersection"
    (let [track (vec (map parse-track-row (track-from-file "day13-example.txt")))]
      (is (= :south (:direction (turn-cart-curve (get-in track [0 4]) (make-cart 4 0 :east)))))
      (is (= :west (:direction (turn-cart-curve (get-in track [0 4]) (make-cart 4 0 :north)))))
      (is (= :north (:direction (turn-cart-curve (get-in track [4 0]) (make-cart 0 4 :west)))))
      (is (= :east (:direction (turn-cart-curve (get-in track [2 2]) (make-cart 2 2 :north)))))
      (is (= :north (:direction (turn-cart-curve (get-in track [4 4]) (make-cart 4 4 :east)))))
      )))

(deftest move-cart-test
  (testing "cart moving"
    (is (= 0 (:y (move-cart (make-cart 4 1 :north)))))
    (is (= 1 (:y (move-cart (make-cart 4 0 :south)))))
    (is (= 5 (:x (move-cart (make-cart 4 0 :east)))))
    (is (= 3 (:x (move-cart (make-cart 4 0 :west)))))
    ))

(deftest find-first-crash-test
  (testing "finding crashes"
    (is (= nil (find-first-crash [(make-cart 0 0 :south) (make-cart 1 1 :north)])))
    (is (= [1 1] (find-first-crash [(make-cart 1 1 :south) (make-cart 1 1 :north)])))
    ))