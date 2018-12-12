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

(defn next-generation [plants combinations]
  (let [until (if (= [nil nil] (take-last 2 plants)) (count plants) (+ 2 (count plants) ))]
    (loop [idx 0
           next []]
      (if-not (= idx until)
        (let [pots [(get plants (- idx 2)) (get plants (dec idx)) (get plants idx) (get plants (inc idx)) (get plants (+ idx 2))]]
          ;(println idx)
          ;(println pots (in? combinations pots))
          (recur (inc idx) (conj next (if (in? combinations pots) \# nil))))
        next))))

(defn live-generations [count initial combinations]
  (loop [gen-cnt count
         generation initial]
    (if-not (= 0 gen-cnt)
      (recur (dec gen-cnt) (next-generation generation combinations))
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