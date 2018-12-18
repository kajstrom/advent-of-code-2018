(ns clj.day18
  (:require [clj.shared :refer :all]))

(defn map-cell-type [cell]
  (case cell
    \. :open
    \| :trees
    \# :lumberyard
    :else :error))

(defn map-cell-type-to-char [cell]
  (case cell
    :open "."
    :trees "|"
    :lumberyard "#"
    :else "E"))

(defn parse-area [file]
  (->> (split-lines-from-file file)
       (map #(seq (char-array %)))
       (map #(vec (map map-cell-type %)))
       vec
       ))

(defn get-adjacent [orig-y orig-x area]
  (let [x-range (range (dec orig-x) (+ 2 orig-x))
        y-range (range (dec orig-y) (+ 2 orig-y))]
    (-> (for [y y-range]
          (for [x x-range]
            (if-not (and (= orig-x x) (= orig-y y))
              (get-in area [y x]))))
        flatten)))

(defn open-next [adjacent]
  (let [adjancent-trees (filter #(= :trees %) adjacent)]
    (if (>= (count adjancent-trees) 3)
      :trees
      :open)))

(defn trees-next [adjacent]
  (let [adjacent-lumberyards (filter #(= :lumberyard %) adjacent)]
    (if (>= (count adjacent-lumberyards) 3)
      :lumberyard
      :trees)))

(defn lumberyard-next [adjacent]
  (let [adjacent-trees (filter #(= :trees %) adjacent)
        adjacent-lumberyards (filter #(= :lumberyard %) adjacent)]
    (if (and (>= (count adjacent-trees) 1) (>= (count adjacent-lumberyards) 1))
      :lumberyard
      :open)))

(defn cell-next [y x cell area]
  (let [adjacent (get-adjacent y x area)]
    (case cell
      :open (open-next adjacent)
      :trees (trees-next adjacent)
      :lumberyard (lumberyard-next adjacent))))

(defn area-next [area]
  (vec (map-indexed (fn [y row]
                      (vec (map-indexed (fn [x cell]
                                          (cell-next y x cell area)) row))) area)))

(defn print-area [area]
  (->> (map (fn [row]
             (map #(map-cell-type-to-char %) row)) area)
      (map #(apply str %))
       )
  )

(defn change-for-minutes [minutes area]
  (loop [minute 0
         area area]
    area
    (if (= minutes minute)
      area
      (recur (inc minute) (area-next area)))))

(defn area-resource-value [area]
  (let [trees (filter #(= :trees %) (flatten area))
        lumberyards (filter #(= :lumberyard %) (flatten area))]
    (* (count trees) (count lumberyards))))

(defn part-1 []
  (let [area (parse-area "day18.txt")]
    (area-resource-value (change-for-minutes 10 area))
    ))

(defn change-with-values-for-minutes [minutes area]
  (loop [minute 0
         area area
         values []]
    area
    (if (= minute minutes)
      values
      (let [new-area (area-next area)
            value (area-resource-value new-area)]
        (recur (inc minute) new-area (conj values value))))))

(defn part-2 []
  (let [area (parse-area "day18.txt")]
    (last (change-with-values-for-minutes 1000 area))
    ))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1))
      (println "Part 2")
      (time (part-2)))))