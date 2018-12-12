(ns clj.day12
  (:require [clojure.string :as s]
            [clj.shared :refer :all]))

(defn parse-plant-string [string]
  (map #(if (= \# %) \#) (vec (char-array string))))

(def test-producing-combinations
  (map parse-plant-string [
                           "...##"
                           "..#.."
                           ".#..."
                           ".#.#."
                           ".#.##"
                           ".##.."
                           ".####"
                           "#.#.#"
                           "#.###"
                           "##.#."
                           "##.##"
                           "###.."
                           "###.#"
                           "####."]))

(defn combinations-from-file []
  (map parse-plant-string (split-lines-from-file "day12.txt")))

(def index-offset 3)

(defn parse-input [string]
  (vec (concat [nil nil nil] (parse-plant-string string))))

(defn pot-next-status [combinations plants idx]
  (let [pots [(get plants (- idx 2)) (get plants (dec idx)) (get plants idx) (get plants (inc idx)) (get plants (+ idx 2))]]
    (if (in? combinations pots) \# nil)))

(defn next-generation [plants combinations]
  (let [until (if (= [nil nil] (take-last 2 plants)) (count plants) (+ 2 (count plants) ))
        indexes (range 0 (inc until))]
    (doall (vec (map #(pot-next-status combinations plants %) indexes)))))

(defn live-generations [gen-cnt initial combinations]
  (loop [gen-counter 0
         generation initial]
    (if-not (= gen-counter gen-cnt)
      (recur (inc gen-counter) (next-generation generation combinations))
      generation)))

(defn sum-generation [offset generation]
  (apply + (map-indexed (fn [idx pot]
                          (if-not (nil? pot)
                            (- idx offset)
                            0)
                          ) generation)))

(defn part-1 []
  (let [input-generation (parse-input "#...#####.#..##...##...#.##.#.##.###..##.##.#.#..#...###..####.#.....#..##..#.##......#####..####...")
        gen20 (live-generations 20 input-generation (combinations-from-file))]
    (sum-generation index-offset gen20)))

(defn live-generations-all-sums [offset gen-cnt initial combinations]
  (loop [gen-counter 0
         generation initial
         sums []]
    (if-not (= gen-counter gen-cnt)
      (let [next (next-generation generation combinations)]
        (recur (inc gen-counter) next (conj sums (sum-generation offset next))))
      sums)))

(defn part-2 []
  (let [input-generation (parse-input "#...#####.#..##...##...#.##.#.##.###..##.##.#.#..#...###..####.#.....#..##..#.##......#####..####...")
        combinations (combinations-from-file)
        all-sums (live-generations-all-sums index-offset 150 input-generation combinations)
        last-2 (take-last 2 all-sums)
        diff (- (last last-2) (first last-2))]
    (+ (* diff (- 50000000000 150)) (last last-2))))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1))
      (println "Part 2")
      (time (part-2))
      )))