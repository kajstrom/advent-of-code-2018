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

(defn manhattan-distance-z [[a b e] [c d f]]
  (Math/abs (+ (Math/abs (- a c)) (Math/abs (- b d)) (Math/abs (- e f)))))

(defn nanobots-in-range-of [largest nanobots]
  (let [x (:x largest)
        y (:y largest)
        z (:z largest)]
    (filter #(>= (:range largest) (manhattan-distance-z [x y z] [(:x %) (:y %) (:z %)])) nanobots)))

(defn nanobot-with-largest-range [nanobots]
  (reduce (fn [carry nb]
            (if (> (:range carry) (:range nb))
              carry
              nb)) nanobots))

(defn part-1 []
  (let [nanobots (parse-nanobots "day23.txt")
        largest-range (nanobot-with-largest-range nanobots)]
    (count (nanobots-in-range-of largest-range nanobots))))