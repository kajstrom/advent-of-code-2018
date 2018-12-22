(ns clj.day22-test
  (:require [clojure.test :refer :all]
            [clj.day22 :refer :all]))

(deftest create-region-test
  (testing "creating 0,0 region"
    (is (= :rocky (:type (create-region 0 0 510 [] [10 10])))))
  (testing "creating a region with 0 y index"
    (is (= :wet (:type (create-region 1 0 510 [] [10 10])))))
  (testing "creating a region with 0 x index"
    (is (= :rocky (:type (create-region 0 1 510 [] [10 10])))))
  (testing "creating a region where calculation is based on other regions"
    (let [regions [[(create-region 0 0 510 [] [10 10]) (create-region 1 0 510 [] [10 10])]
                   [(create-region 0 1 510 [] [10 10])]]]
      (is (= :narrow (:type (create-region 1 1 510 regions [10 10]))))
      ))
  (testing "creating target region"
    (is (= :rocky (:type (create-region 10 10 510 [] [10 10])))))
  )
