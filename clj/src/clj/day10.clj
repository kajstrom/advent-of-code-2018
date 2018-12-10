(ns clj.day10
  (:require [clj.shared :refer [split-lines-from-file parse-int]]
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
            x (+ (Math/abs x-offset) (first pos))
            y (+ (Math/abs y-offset) (second pos))
            ]
        (recur (assoc-in grid [y x] "#") (rest points)))
      (doseq [row grid] (spit "file.txt" (str (apply str row) "\n") :append true))
      ;(count grid)
      )))

(defn move-seconds [seconds points]
  (dotimes [n seconds]
    (doall (map move-point points)))
  (let [smallest-largest (smallest-and-largest-coords points)
        x-offset (first (first smallest-largest))
        y-offset (first (last smallest-largest))
        grid (make-grid (grid-size smallest-largest))]
    (set-points-to-grid grid points x-offset y-offset)))

(defn part-1-and-2 []
  (let [points (read-points)]
    ;Trial and error to find the range where the points start to come together
    ;And then looking up the exact second manually
    ;Couldn't think of a finer solution to find this :(
    (move-seconds 10932 points))
  )