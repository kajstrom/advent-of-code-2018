(ns clj.day8
  (:require [clj.shared :refer [parse-int]]
            [clojure.string :as s]
            [clojure.java.io :refer [resource]]))

(defn load-numbers [file]
  (map parse-int (-> (resource file)
                     slurp
                     (s/split #" "))))

(defn build-tree [numbers]
  (let [node (atom {:children [] :meta []})
        [child-cnt meta-cnt] (take 2 @numbers)]
    (reset! numbers (drop 2 @numbers))
    (dotimes [n child-cnt]
      (swap! node update :children conj (build-tree numbers)))
    (swap! node assoc :meta (take meta-cnt @numbers))
    (reset! numbers (drop meta-cnt @numbers))
    @node))

(defn sum-node [node]
  (apply + (concat (:meta node) (for [child (:children node)] (sum-node child)))))

(defn part-1 []
  (let [tree (build-tree (atom (load-numbers "day8.txt")))]
    (sum-node tree)))

(defn value-of-node [node]
  (if-not (empty? (:children node))
    (do
      (let [children (:children node)
            child-cnt (count children)
            meta (:meta node)]
        (apply + (for [child-idx meta] (if (> child-idx child-cnt)
                                         0
                                         (value-of-node (nth children (dec child-idx))))))))
    (apply + (:meta node))))

(defn part-2 []
  (let [tree (build-tree (atom (load-numbers "day8.txt")))]
    (value-of-node tree)))

(defn time-results []
  (time
    (do
      (println "Part 1:")
      (time (part-1))
      (println "Part 2:")
      (time (part-2)))))