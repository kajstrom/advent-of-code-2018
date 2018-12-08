(ns clj.day3
  (:require [clojure.string :as s]
            [clj.shared :refer [parse-int split-lines-from-file not-in?]]
            [clojure.data :refer [diff]]))

;#1 @ 108,350: 22x29
(defn parse-claim-string [claim]
  (let [splitted (s/split claim #" ")
        coords (->> (s/split  (s/replace (nth splitted 2) ":" "") #",") (map parse-int))
        dimensions (->> (s/split (last splitted) #"x") (map parse-int))
        id (first splitted)]
    (hash-map
      :id id
      :width (first dimensions)
      :height (second dimensions)
      :col (first coords)
      :row (second coords)
      )))

(defn create-fabric [size]
  (vec (replicate size (vec (replicate size [])))))

(defn claim-area [fabric claim]
  (let [id (:id claim)
        col (:col claim)
        row (:row claim)
        height (:height claim)
        width (:width claim)
        max-col (+ col width)
        max-row (+ row height)
        rows (range row max-row 1)
        columns (range col max-col 1)]
    (doseq [row rows]
      (doseq [col columns]
        (reset! fabric (update-in @fabric [row col] conj id))))))

(defn claimed-fabric [claims]
  (let [fabric (atom (create-fabric 1000))]
    (doseq [claim claims]
      (claim-area fabric claim))
    @fabric))

(defn part-1 [claims]
    (->> (map (fn [row] (filter #(<= 2 (count %)) row)) (claimed-fabric claims))
         (map count)
         (apply +)))

(defn find-overlapping-claims [fabric]
  (->> (map (fn [row] (filter #(<= 2 (count %)) row)) fabric)
       flatten
       distinct))

(defn part-2 [claims]
  (let [ids (map :id claims)
        overlapping (find-overlapping-claims (claimed-fabric claims))]
    (filter #(not-in? overlapping %) ids)))

(defn time-results []
  (let [claims (->> (split-lines-from-file "day3.txt") (map parse-claim-string))]
    (time (part-1 claims))
    (time (part-2 claims))))