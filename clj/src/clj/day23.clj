(ns clj.day23
  (:require [clj.shared :refer :all]
            [clojure.string :as s]))

(defn parse-nanobot [row]
  (let [[x y z] (map parse-int (-> (s/split row #"<|>") second (s/split #",") ))
        range (parse-int (-> (s/split row #"r=") last))]
    {:x x :y y :z z :range range}))

(defn parse-nanobots [file]
  (->> (split-lines-from-file file)
      (map parse-nanobot)))

(defn nanobots-in-range-of [largest nanobots]
  (let [x (:x largest)
        y (:y largest)
        z (:z largest)]
    (filter #(>= (:range largest) (manhattan-distance [x y z] [(:x %) (:y %) (:z %)])) nanobots)))

(defn nanobot-with-largest-range [nanobots]
  (reduce (fn [carry nb]
            (if (> (:range carry) (:range nb))
              carry
              nb)) nanobots))

(defn part-1 []
  (let [nanobots (parse-nanobots "day23.txt")
        largest-range (nanobot-with-largest-range nanobots)]
    (count (nanobots-in-range-of largest-range nanobots))))

; !!!!NOTE!!!!! Part 2 solution is based on https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecddus1
; Used this as a learning experience as I didn't really know even where to start with solving this.

(defn nanobots-in-range-of-pos [[x y z] nanobots]
  (let []
    (filter #(>= (:range %) (manhattan-distance [x y z] [(:x %) (:y %) (:z %)])) nanobots)))

(defn make-search-distance [min-x max-x]
  (loop [distance 1]
    (if (< distance (- max-x min-x))
      (recur (* distance 2))
      distance)))

(defn search [nanobots distance x-coords y-coords z-coords]
  (let [min-x (apply min x-coords)
        max-x (apply max x-coords)
        min-y (apply min y-coords)
        max-y (apply max y-coords)
        min-z (apply min z-coords)
        max-z (apply max z-coords)
        step distance
        coords (transient [])]
    (doseq [x (range min-x (inc max-x) step)]
      (doseq [y (range min-y (inc max-y) step)]
        (doseq [z (range min-z (inc max-z) step)]
          (let [bots-in-range (nanobots-in-range-of-pos [x y z] nanobots)
                bot-cnt (count bots-in-range)]
            (conj! coords [[x y z] bot-cnt])))))
    (persistent! coords)))

(defn find-best-result [results]
  (let [max-bots-in-range (apply max (map last results))
        results-with-max (filter #(= max-bots-in-range (last %)) results)]
    (if (> (count results-with-max) 1)
      (let [best-distance (reduce (fn [carry res]
                                    (if (< (manhattan-distance [0 0 0] (first res)) (manhattan-distance [0 0 0] (first carry)))
                                      res
                                      carry)) results)]
        best-distance)
      (first results-with-max))))

(defn new-coords-by-distance [current-dist [x y z]]
  [[(- x current-dist) (+ x current-dist)]
   [(- y current-dist) (+ y current-dist)]
   [(- z current-dist) (+ z current-dist)]])

(defn coords-in-range-of-most-nanobots [nanobots]
  (let [min-x (apply min (map :x nanobots))
        max-x (apply max (map :x nanobots))
        distance (make-search-distance min-x max-x)
        ]
    (loop [current-most-targets [0 0 0]
           current-target-cnt 0
           current-dist-to-0 999999999999
           distance distance
           x-coords (map :x nanobots)
           y-coords (map :y nanobots)
           z-coords (map :z nanobots)]
      (if (> 1 distance)
        [current-most-targets current-target-cnt]
        (do
          (let [search-results (search nanobots distance x-coords y-coords z-coords)
                [coords cnt] (find-best-result search-results)
                new-dist (/ distance 2)
                [x-coords y-coords z-coords] (new-coords-by-distance new-dist current-most-targets)]
            (if (> cnt current-target-cnt)
              (let [[x-coords y-coords z-coords] (new-coords-by-distance new-dist coords)]
                (recur coords cnt (manhattan-distance [0 0 0] coords) new-dist x-coords y-coords z-coords))
              (if (>= cnt current-target-cnt)
                (let [new-dist-to-0 (manhattan-distance [0 0 0] coords)]
                  (if (< new-dist-to-0 current-dist-to-0)
                    (let [[x-coords y-coords z-coords] (new-coords-by-distance new-dist coords)]
                      (recur coords cnt new-dist-to-0 new-dist x-coords y-coords z-coords))
                    (recur current-most-targets current-target-cnt current-dist-to-0 new-dist x-coords y-coords z-coords)))
                (recur current-most-targets current-target-cnt current-dist-to-0 new-dist x-coords y-coords z-coords))))
          ))
      )))

(defn scan-closest [[x y z] max-targets nanobots]
  (let [scan-range 3
        min-x (- x scan-range)
        max-x (+ x scan-range)
        min-y (- y scan-range)
        max-y (+ y scan-range)
        min-z (- z scan-range)
        max-z (+ z scan-range)
        closest-to-0 (atom [x y z])]
    (doseq [x (range min-x (inc max-x))]
      (doseq [y (range min-y (inc max-y))]
        (doseq [z (range min-z (inc max-z))]
          (let [targets-in-range (nanobots-in-range-of-pos [x y z] nanobots)]
            (if (= max-targets (count targets-in-range))
              (if (< (manhattan-distance [0 0 0] [x y z]) (manhattan-distance [0 0 0] @closest-to-0))
                (reset! closest-to-0 [x y z])))))))
    @closest-to-0))

(defn part-2 []
  (let [nanobots (parse-nanobots "day23.txt")
        closest (coords-in-range-of-most-nanobots nanobots)]
    (manhattan-distance [0 0 0] (scan-closest (first closest) (last closest) nanobots))
    ))