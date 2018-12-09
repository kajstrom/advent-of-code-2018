(ns clj.day9-test
  (:require [clojure.test :refer :all]
            [clj.day9 :refer :all]))

(deftest add-marble-next-to-test
  (testing "adds marble next to"
    (let [circle (make-marble 0)
          marble (add-marble-next-to circle (make-marble 1))]
      (is (= 1 (:value marble))))))

(deftest remove-marble-test
  (testing "removes marble"
    (let [circle (make-marble 0)]
      (add-marble-next-to circle (make-marble 1))
      (is (= 1 (:value (remove-marble circle)))))
    (let [circle (make-marble 0)
          added (add-marble-next-to circle (make-marble 1))]
      (is (= 0 (:value (remove-marble added))))
      )))

(deftest move-forward-test
  (testing "moves marble"
    (let [circle (make-marble 0)
          marble (add-marble-next-to circle (make-marble 1))]
      (is (= 1 (:value (move-forward circle))))
      (is (= 0 (:value (move-forward marble)))))))

(deftest move-backwards-test
  (testing "moves marble"
    (let [circle (make-marble 0)
          marble (-> (add-marble-next-to circle (make-marble 1))
                     (add-marble-next-to (make-marble 2))
                     (add-marble-next-to (make-marble 3))
                     (add-marble-next-to (make-marble 4)))]

      (is (= 2 (:value (move-backwards marble)))))
    (let [circle (make-marble 0)
          marble (-> (add-marble-next-to circle (make-marble 1))
                     (add-marble-next-to (make-marble 2))
                     (add-marble-next-to (make-marble 3))
                     (add-marble-next-to (make-marble 4))
                     (add-marble-next-to (make-marble 5))
                     (add-marble-next-to (make-marble 6))
                     (add-marble-next-to (make-marble 7))
                     )])))