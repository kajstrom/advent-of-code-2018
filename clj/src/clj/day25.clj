(ns clj.day25
  (:require [clj.shared :refer :all]
            [clojure.string :as s]))

(defn parse-input [file]
  (->> (split-lines-from-file file)
       (map s/trim)
       (map #(s/split % #","))
       (map #(map parse-int %))))

(defn spacetime-distance [[a b c d] [e f g h]]
  (+ (manhattan-distance [a b c] [e f g]) (Math/abs (- d h))))

(defn connects [to test]
  (<= (spacetime-distance to test) 3))

(defn find-constellations [points]
  (loop [current [(first points)]
         idx 0
         constellations []
         points (rest points)]
    (if (empty? points)
      (conj constellations current)
      (let [constellations (if (>= idx (count current)) (conj constellations current) constellations)
            current (if (>= idx (count current)) [(first points)] current)
            points (if (>= idx (count current)) (rest points) points)
            idx (if (>= idx (count current)) 0 idx)
            point (get current idx)
            connecting (filter (partial connects point) points)
            remaining-points (filter #(not-in? connecting %) points)]
        ;(println point)
        (recur (apply conj current connecting) (inc idx) constellations remaining-points)))))

(defn part-1 []
  (let [points (parse-input "day25.txt")]
    (-> (find-constellations points) count)))