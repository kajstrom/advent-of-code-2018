(ns clj.day10
  (:require [clj.shared :refer :all]
            [clojure.string :as s]))

(defn read-points []
  (->> (split-lines-from-file "day10.txt")
      (map #(s/split % #"<|>"))
      (map #(vec [(second %) (last %)]))
       (map (fn [[pos vel]]
              [(->> (s/split pos #",") (map #(s/trim %)) (map parse-int))
               (->> (s/split vel #",") (map #(s/trim %)) (map parse-int))
               ]))
       (map #(transient {:pos (first %) :vel (last %)}))))

(defn move-point [point]
  (let [[pos-x pos-y] (:pos point)
        [vel-x vel-y] (:vel point)]
    (assoc! point :pos [(+ pos-x vel-x) (+ pos-y vel-y)])))

(defn smallest-and-largest-coords [points]
  (let [min-x (apply min (map #(first (:pos %)) points))
        max-x (apply max (map #(first (:pos %)) points))
        min-y (apply min (map #(last (:pos %)) points))
        max-y (apply max (map #(last (:pos %)) points))]
    [[min-x max-x] [min-y max-y]]))

(defn grid-size [max-coords]
  (let [x (apply + (map #(Math/abs %) (first max-coords)))
        y(apply + (map #(Math/abs %) (last max-coords)))]
    [x y]))

(defn make-grid [[x y]]
  (vec (repeat (inc y) (vec (repeat (inc x) ".")))))

(defn set-points-to-grid [grid points x-offset y-offset]
  (loop [grid grid
         points points]
    (if-not (empty? points)
      (let [pos (:pos (first points))
            x (- (first pos) (Math/abs x-offset))
            y (- (second pos) (Math/abs y-offset))
            ]
        (recur (assoc-in grid [y x] "#") (rest points)))
      (doseq [row grid] (println (apply str row)))
      )))

(defn has-adjacent-points [points point]
  (let [[x y] (:pos point)
        top-left [(dec x) (dec y)]
        top [x (dec y)]
        top-right [(inc x) (dec y)]
        left [(dec x) y]
        right [(inc x) y]
        bottom-left [(dec x) (inc y)]
        bottom [x (inc y)]
        bottom-right [(inc x) (inc y)]
        adjacent [top-left top top-right left right bottom-left bottom bottom-right]]
    (some (fn [a-point]
            (in? (map :pos points) a-point)) adjacent)))

(defn all-have-adjacent-points? [points]
  (every? (partial has-adjacent-points points) points))

(defn move-until-all-have-adjacent [points]
  (loop [second 0]
    (if (all-have-adjacent-points? points)
      (let [smallest-largest (smallest-and-largest-coords points)
            x-offset (first (first smallest-largest))
            y-offset (first (last smallest-largest))
            grid (make-grid (grid-size smallest-largest))]
        (println "Seconds" second)
        (set-points-to-grid grid points x-offset y-offset))
      (do
        (doall (map move-point points))
        (recur (inc second))))))

(defn part-1-and-2 []
  (let [points (read-points)]
    (move-until-all-have-adjacent points)
    ))