(ns clj.day9-test
  (:require [clojure.test :refer :all]
            [clj.day9 :refer :all]))

(deftest add-marble-next-to-test
  (testing "adds marble next to"
    (let [circle (make-marble 0 nil)
          marble (add-marble-next-to circle (make-marble 1 nil))]
      (is (= 2 (circle-len circle)))
      (is (= 1 (:value marble))))))

(deftest remove-marble-test
  (testing "removes marble"
    (let [circle (make-marble 0 nil)]
      (add-marble-next-to circle (make-marble 1 nil))
      (is (= 1 (:value (remove-marble circle)))))
    (let [circle (make-marble 0 nil)
          added (add-marble-next-to circle (make-marble 1 nil))]
      (is (= nil (:value (remove-marble added))))           ; This nil return might have caused problems...
      )))

(deftest move-forward-test
  (testing "moves marble"
    (let [circle (make-marble 0 nil)
          marble (add-marble-next-to circle (make-marble 1 nil))]
      (is (= 1 (:value (move-forward circle circle))))
      (is (= 0 (:value (move-forward circle marble)))))))

(deftest move-backwards-test
  (testing "moves marble"
    (let [circle (make-marble 0 nil)
          marble (-> (add-marble-next-to circle (make-marble 1 nil))
                     (add-marble-next-to (make-marble 2 nil))
                     (add-marble-next-to (make-marble 3 nil))
                     (add-marble-next-to (make-marble 4 nil)))]

      (is (= 2 (:value (move-backwards circle marble)))))
    (let [circle (make-marble 0 nil)
          marble (-> (add-marble-next-to circle (make-marble 1 nil))
                     (add-marble-next-to (make-marble 2 nil))
                     (add-marble-next-to (make-marble 3 nil))
                     (add-marble-next-to (make-marble 4 nil))
                     (add-marble-next-to (make-marble 5 nil))
                     (add-marble-next-to (make-marble 6 nil))
                     (add-marble-next-to (make-marble 7 nil))
                     )])))