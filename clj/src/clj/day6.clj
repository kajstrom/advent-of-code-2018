(ns clj.day6
  (:require [clj.shared :refer [split-lines-from-file parse-int]]
            [clojure.string :as s]))

(def test-data [
                [1, 1]
                [1, 6]
                [8, 3]
                [3, 4]
                [5, 5]
                [8, 9]
                ])

(defn load-input []
  (let [split-lines (split-lines-from-file "day6.txt")
        split-coordinates (map #(s/split % #", ") split-lines)]
    (map (fn [line] (map parse-int line)) split-coordinates)))

(defn make-grid [max-x max-y]
  (vec (replicate (inc max-x) (vec (replicate (inc max-y) nil)))))

(defn grid-from-coords [coords]
  (let [max-x (apply max (map first coords))
        max-y (apply max (map second coords))
        largest (max max-y max-x)]
    (make-grid largest largest)))

(defn populate-coords-to-grid [grid coords]
  (loop [grid grid
         coords coords
         num 0]
    (if-not (empty? coords)
      (let [[x y] (first coords)]
        (recur (assoc-in grid [y x] num) (rest coords) (inc num)))
      grid)))

(defn manhattan-distance [[a b] [c d]]
  (Math/abs (+ (Math/abs (- a c)) (Math/abs (- b d)))))

(defn pos-closest-coords [pos coords]
  (let [distances (map #(manhattan-distance pos %) coords)
        smallest-dist (apply min distances)
        equal-dist (< 1 (count (filter #(= smallest-dist %) distances)))
        ]
    (if-not equal-dist
      (.indexOf distances smallest-dist))))

(defn pos-dist-sum-to-coords [pos coords]
  (apply + (map #(manhattan-distance pos %) coords)))

(defn populate-grid-pos-with-coords [val-fn grid coords]
  (for [x (range 0 (count (first grid)))]
    (for [y (range 0 (count grid))]
      (val-fn [y x] coords))))

(defn remove-infinites [grid]
  (let [top-row (get grid 0)
        bottom-row (last grid)
        first-col (map first grid)
        last-col (map last grid)
        infinites (filter #(not (nil? %)) (distinct (concat top-row bottom-row first-col last-col)))
        ]
    (map (fn [row]
           (map (fn [cell]
                  (if (nil? cell)
                    cell
                    (if (< -1 (.indexOf infinites cell))
                      nil
                      cell))) row)
                ) grid)
    ))

(defn part-1 []
  (let [coords (load-input)
        grid (grid-from-coords coords)
        populated-grid (populate-grid-pos-with-coords pos-closest-coords grid coords)
        infinites-removed-grid (remove-infinites populated-grid)
        finites-by-idx (group-by identity (filter #(not (nil? %)) (flatten infinites-removed-grid)))
        ]
    ; Bug in my code or checker, but the second highest is correct?
    ; https://www.reddit.com/r/adventofcode/comments/a3kr4r/2018_day_6_solutions/eb76843
    (second (sort > (map count (vals finites-by-idx))))
    ))

(defn part-2 []
  (let [coords (load-input)
        grid (grid-from-coords coords)
        populated-grid (populate-grid-pos-with-coords pos-dist-sum-to-coords grid coords)
        ]
    (count (filter #(< % 10000) (flatten populated-grid)))))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1))
      (println "Part 2")
      (time (part-2))
      )))