(ns clj.day7-test
  (:require [clojure.test :refer :all]
            [clj.day7 :refer :all]))

(reset! step-add-value 0)

(deftest find-root-test
  (testing "finds root step"
    (let [reqs (load-requirements "day7-example.txt")
          steps (steps-from-requirements reqs)]
      (is (= '(\C) (find-root reqs steps))))))

(deftest next-non-traversed-test
  (testing "finds next non-traversed"
    (is (= {:step \A :pre-req '(\C)} (next-non-traversed [{:step \A :pre-req '(\C)} {:step \F :pre-req '(\C)}] [\C])))
    (is (= {:step \F :pre-req '(\C)} (next-non-traversed [{:step \A :pre-req '(\C)} {:step \F :pre-req '(\C)}] [\C \A])))
    (is (= nil (next-non-traversed [{:step \A :pre-req '(\C)} {:step \F :pre-req '(\C)}] [\C \A \F])))))

(deftest find-free-workers-test
  (testing "finds free worker"
    (is (= [0 1]  (find-free-workers [[] []] [])))
    (is (= [1] (find-free-workers [[] []] [{:step \C :worker 0}]))))
  (testing "returns empty when no workers available"
    (is (= [] (find-free-workers [[] []] [{:step \C :worker 0} {:step \B :worker 1}])))))

(deftest find-completed-steps-test
  (testing "finds completed steps"
    (is (= [{:step \C :seconds-of-work []}] (find-completed-steps [{:step \C :seconds-of-work []}])))
    (is (empty? (find-completed-steps [{:step \C :seconds-of-work [1 2 3 4 5 6]}])))))

(deftest remove-completed-steps-test
  (testing "removes completed steps"
    (is (empty? (remove-completed-steps [{:step \C}] [{:step \C}])))
    (is (not (empty? (remove-completed-steps [{:step \D}] [{:step \C}]))))))

(deftest remove-pre-reqs-test
  (testing "removes completed from pre-req"
    (is (= [{:step \C :pre-req []}] (remove-pre-reqs [{:step \B}] [{:step \C :pre-req [\B]}])))
    (is (= [{:step \C :pre-req [\B]}] (remove-pre-reqs [{:step \A}] [{:step \C :pre-req [\B]}])))))

(deftest find-available-steps-test
  (testing "finds available steps"
    (is (empty? (find-available-steps [{:step \B :pre-req [\C]}])))
    (is (= [{:step \C :pre-req []} {:step \D :pre-req []}] (find-available-steps [{:step \D :pre-req []} {:step \C :pre-req []}])))))

(deftest assign-steps-to-workers-test
  (testing "assigns steps"
    (is (= [{:step \C :seconds-of-work [0 1 2] :worker 0}] (assign-steps-to-workers [0] [{:step \C}] [])))
    (is (= [{:step \B} {:step \C :seconds-of-work [0 1 2] :worker 0}] (assign-steps-to-workers [0] [{:step \C}] [{:step \B}])))
    (is (= [{:step \C :seconds-of-work [0 1 2] :worker 0}] (assign-steps-to-workers [0] [{:step \C} {:step \D}] [])))
    ))

(deftest increment-worker-timelines-test
  (testing "increments worker timelines"
    (is (= [[0] [nil]] (increment-worker-timelines [[] []] [{:step \C :seconds-of-work [0 1 2] :worker 0}])))))

(deftest decrement-work-left-test
  (testing "decrements work left"
    (is (= [{:step \C :seconds-of-work [1 2]}] (decrement-work-left [{:step \C :seconds-of-work [0 1 2]}])))))

(deftest remove-assigned-from-steps-left-test
  (testing "removes assigned from steps left"
    (is (empty? (remove-assigned-from-steps-left [{:step \C}] [{:step \C}])))))
