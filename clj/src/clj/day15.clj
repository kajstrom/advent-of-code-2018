(ns clj.day15
  (:require [clj.shared :refer :all]))

(def unit-id-counter (atom 0))
(defn make-unit [type x y]
  (let [id @unit-id-counter]
    (swap! unit-id-counter inc)
    {:type type :hp 200 :ap 3 :x x :y y :id id}))

(def make-goblin (partial make-unit :goblin))
(def make-elf (partial make-unit :elf))

(defn parse-map-and-units [file]
  (let [lines (map #(seq (char-array %)) (split-lines-from-file file))
        units (transient [])
        cavern-map (vec (map-indexed
                          (fn [y line]
                            (vec (map-indexed
                                   (fn [x cell]
                                     (case cell
                                       \# :wall
                                       \. :free
                                       \G (let [goblin (make-goblin x y)]
                                            (conj! units goblin)
                                            goblin)
                                       \E (let [elf (make-elf x y)]
                                            (conj! units elf)
                                            elf)
                                       )) line))) lines))]
    [cavern-map (persistent! units)]))

(defn find-free-cells-next-to
  "Note returns coords in y,x order! For easier access. Cells are returned in reading order"
  [[y x] cavern-map]
  (let [adjacent-cells [[ (dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]]]
    (filter #(not= nil %) (map (fn [coords]
                                 (let [cell (get-in cavern-map coords)]
                                   (if (= :free cell)
                                     coords))) adjacent-cells))))

(defn filter-equal-or-less-cells [new-cells path]
  (filter (fn [[y x c]]
            (some #(let [[p-y p-x p-c] %]
                     (if (and (= p-y y) (= p-x x))
                       (<= p-c c)
                       true)
                     ) path)) new-cells))

(defn shortest-path-to
  "Note takes and returns coords in y,x order!"
  [orig dest cavern-map]
  ;(println "From" orig "To" dest)
  (loop [counter 0
         path [(conj dest counter)]
         idx 0]
    ;(println (take 2 (last path)))
    (if-not (> counter (* 2 (count cavern-map)))
      (if-not (= idx (count path))
        (let [counter (inc counter)
              current (get path idx)
              adjacent-cells (find-free-cells-next-to (take 2 current) cavern-map)
              adjacent-with-count (map #(conj % counter) adjacent-cells)
              cells-to-visit (filter-equal-or-less-cells adjacent-with-count path)
              ]
          ;(println counter path current adjacent-with-count)
          (if (empty? cells-to-visit)
            (recur counter path (inc idx))
            (if (some #(= orig (vec (take 2 %))) cells-to-visit)
              (conj path (conj orig counter))
              (recur counter (apply conj path cells-to-visit) (inc idx))))))
      (do
        ;(println "End reached" path)
        nil))))

(defn available-in-range-paths [unit units cavern-map]
  (let [target-type (if (= :elf (:type unit)) :goblin :elf)
        targets (filter #(= target-type (:type %))  units)
        cells-in-range-of-targets (flatten-1 (map #(find-free-cells-next-to [(:y %) (:x %)] cavern-map) targets))
        cells-next-to-unit (find-free-cells-next-to [(:y unit) (:x unit)] cavern-map)]
    (println cells-next-to-unit)
    (->> (map (fn [cell]
                (map #(shortest-path-to cell % cavern-map) cells-in-range-of-targets)
                ) cells-next-to-unit)
         flatten-1
         (filter #(not= nil %)))
    ))

(defn shortest-paths [paths]
  (let [shortest-path-length (apply min (map #(last (last %)) paths))]
    (filter #(= shortest-path-length (-> % last last)) paths)))

(defn choose-next-move-destination [unit units cavern-map]
  (let [cells-next-to-unit (find-free-cells-next-to [(:y unit) (:x unit)] cavern-map)
        available-paths (available-in-range-paths unit units cavern-map)
        ]
    (println available-paths)
    (if-not (empty? available-paths)
      (->> (first (shortest-paths available-paths)) last (take 2) vec))
    ))

(defn part-1 []
  (let [[cavern-map units] (parse-map-and-units "day15-example2.txt")]
    (choose-next-move-destination (nth units 0) units cavern-map)
    ;(shortest-path-to [1 3] [1 2] cavern-map)
    ))