(ns clj.day20-test
  (:require [clojure.test :refer :all]
            [clj.day20 :refer :all]))

(deftest shortest-path-test
  (testing "path calculation"
    (is (= 3 (last (shortest-path 1 "^WNE$"))))
    (is (= 10 (last (shortest-path 1 "^ENWWW(NEEE|SSE(EE|N))$"))))
    (is (= 18 (last (shortest-path 1 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"))))
    (is (= 23 (last (shortest-path 1 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"))))
    (is (= 31 (last (shortest-path 1 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"))))
    ))

(deftest old-part-1-test
  (testing "correct solution"
    (is (= 3512 (old-part-1)))))
