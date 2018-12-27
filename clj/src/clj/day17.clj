(ns clj.day17
  (:require [clj.shared :refer :all]
            [clojure.string :as s]))

(defn parse-number-or-range [string]
  (let [range? (.contains string "..")]
    (if range?
      (let [nums (s/split string #"\.\.")
            [start end] (map parse-int nums)]
        (range start (inc end)))
      (parse-int string))))

(defn parse-coords [coords]
  (let [x (subs (first (filter #(= \x (first %)) coords)) 2)
        y (subs (first (filter #(= \y (first %)) coords)) 2)]
        {:x (parse-number-or-range x) :y (parse-number-or-range y)}))

(defn parse-input [file]
  (->> (split-lines-from-file file)
       (map #(s/split % #", "))
       (map parse-coords)))

(defn make-area [max-y]
  (transient (vec (for [y (range 0 (inc max-y))]
                    (transient (vec (for [x (range 0 1001)]
                                      :sand)))))))

(defn populate-clay [clay-spots area]
  (doseq [clay clay-spots]
    (let [x-seq? (seq? (:x clay))]
      (if x-seq?
        (let [x-range (:x clay)
              y-row (get area (:y clay))]
          (doseq [x x-range]
            (assoc! y-row x :clay)))
        (let [y-range (:y clay)
              x (:x clay)]
          (doseq [y y-range]
            (assoc! (get area y) x :clay)))))))

(defn print-area [area]
  (let []
    (doseq [row (persistent! area)]
      (apply println (map #(case % :sand "." :clay "#" :spring "+" :water-flow "|" :water-still "~") (persistent! row))))))

(defn output-area [area file]
  (doseq [row (persistent! area)]
    (let [row-text (apply str (map #(case % :sand "." :clay "#" :spring "+" :water-flow "|" :water-still "~") (persistent! row)))]
      (spit file (str row-text "\r\n") :append true))))

(defn get-point [y x area]
  (let [point (get (get area y []) x :sand)]
    point))

(defn is-flowing-water [y x area]
  (= :water-flow (get-point y x area)))

(defn can-flow-down-from [y x area]
  (in? [:sand :water-flow] (get-point (inc y) x area)))

(defn can-flow-left-from [y x area]
  (in? [:sand :water-flow] (get-point y (dec x) area)))

(defn can-flow-right-from [y x area]
  (in? [:sand :water-flow] (get-point y (inc x) area)))

(defn update-spot [area y x type]
  (assoc! (get area y) x type))

(defn update-range [area [f-y f-x] [t-y t-x] type]
  (doseq [x (range f-x (inc t-x))]
    (update-spot area f-y x type)))

(defn flow-left [[y x] area]
  (loop [y y
         x x]
    (if-not (can-flow-down-from y x area)
      (if (can-flow-left-from y x area)
        (recur y (dec x))
        [:flow-until [y x]])
      [:trickle-down-from [y x]])))

(defn flow-right [[y x] area]
  ;(println y x (can-flow-down-from y x area))
  (loop [y y
         x x]
    (if-not (can-flow-down-from y x area)
      (if (can-flow-right-from y x area)
        (recur y (inc x))
        [:flow-until [y x]])
      [:trickle-down-from [y x]])))

(defn flow-from [[y x] area]
  (if (is-flowing-water (inc y) x area)
    (do
      (update-spot area y x :water-flow)
      [])
    (if (can-flow-down-from y x area)
      (do
        (update-spot area y x :water-flow)
        [[(inc y) x]]
        )
      (let [[type-left point-left] (flow-left [y x] area)
            [type-right point-right] (flow-right [y x] area)]
        (if (and (= :flow-until type-left) (= :flow-until type-right))
          (do
            ;(println point-left point-right)
            (update-range area point-left point-right :water-still)
            [[(dec y) x]])
          (if (and (= :trickle-down-from type-left) (= :trickle-down-from type-right))
            (do
              ;(print-area area)
              ;(println point-left point-right)
              (update-range area point-left point-right :water-flow)
              ;(print-area area)
              [point-left point-right])
            (if (= :trickle-down-from type-right)
              (do
                (update-range area point-left point-right :water-flow)
                ;(print-area area)
                ;(println point-right)
                [point-right])
              (if (= :trickle-down-from type-left)
                (do
                  (update-range area point-left point-right :water-flow)
                  ;(print-area area)
                  [point-left])
                []))))))))

(defn flow-water [area max-y]
  (loop [point [1 500]
         flow-points []]
    (if (nil? point)
      area
      (do
        (if-let [new-points (flow-from point area)]
          (let [new-flow-points (doall (apply conj flow-points new-points))
                new-flow-points (filter #(and (<= (first %) max-y) (>= (first %) 0)) new-flow-points)]
            (recur (first new-flow-points) (rest new-flow-points)))
          (recur (first flow-points) (rest flow-points)))
        ))))

(defn part-1 []
  (let [clay-spots (parse-input "day17.txt")
        max-y (apply max (flatten (map :y clay-spots)))
        min-y (apply min (flatten (map :y clay-spots)))
        area (make-area max-y)]
    (assoc! (get area 0) 500 :spring)
    (populate-clay clay-spots area)
    (flow-water area max-y)
    ;(output-area area "d17.txt")
    (-> (filter #(or (= % :water-still) (= % :water-flow)) (flatten (drop min-y (map persistent! (persistent! area)))))
        count)))

(defn part-2 []
  (let [clay-spots (parse-input "day17.txt")
        max-y (apply max (flatten (map :y clay-spots)))
        area (make-area max-y)]
    (assoc! (get area 0) 500 :spring)
    (populate-clay clay-spots area)
    (flow-water area max-y)
    (-> (filter #(= % :water-still) (flatten (map persistent! (persistent! area))))
        count)))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1))
      (println "Part 2")
      (time (part-2)))))