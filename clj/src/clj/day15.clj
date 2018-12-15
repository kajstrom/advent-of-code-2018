(ns clj.day15
  (:require [clj.shared :refer :all]))

(def unit-id-counter (atom 0))
(defn make-unit [type x y]
  (let [id @unit-id-counter]
    (swap! unit-id-counter inc)
    (transient {:type type :hp 200 :ap 3 :x x :y y :id id})))

(def make-goblin (partial make-unit :goblin))
(def make-elf (partial make-unit :elf))

(defn enemy-type [unit]
  (if (= :elf (:type unit)) :goblin :elf))

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

(defn list-adjacent-cells
  "Unfiltered adjacent cells, might be negative or occupied"
  [[y x]]
  [[(dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]])

(defn find-free-cells-next-to
  "Note returns coords in y,x order! For easier access. Cells are returned in reading order"
  [[y x] cavern-map]
  (let [adjacent-cells (list-adjacent-cells [y x])]
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
  (if (= orig dest)
    [(conj orig 0)]
    (loop [counter 0
           path [(conj dest counter)]
           idx 0]
      ;(println (take 2 (last path)))
      (if-not (> counter (* 100 (count cavern-map)))
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
                (do
                  ;(println "Found path to" orig)
                  (conj path (conj orig counter)))
                (recur counter (apply conj path cells-to-visit) (inc idx))))))
        (do
          (println "End reached, path not found")
          nil)))))

(defn available-in-range-paths [unit units cavern-map]
  (let [target-type (enemy-type unit)
        targets (filter #(= target-type (:type %))  units)
        cells-in-range-of-targets (flatten-1 (map #(find-free-cells-next-to [(:y %) (:x %)] cavern-map) targets))
        cells-next-to-unit (find-free-cells-next-to [(:y unit) (:x unit)] cavern-map)]
    ;(println cells-next-to-unit cells-in-range-of-targets)
    (->> (map (fn [cell]
                (map #(shortest-path-to cell % cavern-map) cells-in-range-of-targets)
                ) cells-next-to-unit)
         (map (fn [cell-paths]
                (filter #(not= nil %) cell-paths)))
         (filter #(not (empty? %))))
    ))

(defn shortest-paths-per-cell [paths-cell]
  (->> (map (fn [paths]
             (let [shortest-path-length (apply min (map #(last (last %)) paths))]
               (filter #(= shortest-path-length (-> % last last)) paths)))
           paths-cell)
      (map first)))

(defn shortest-path [paths]
  ;(println paths)
  (let [shortest-length (apply min (map #(last (last %)) paths))]
    (->> (filter #(= shortest-length (-> % last last)) paths)
         first)))

(defn choose-next-move-destination [unit units cavern-map]
  (let [cells-next-to-unit (find-free-cells-next-to [(:y unit) (:x unit)] cavern-map)
        available-paths (available-in-range-paths unit units cavern-map)
        ]
    ;(println (count available-paths))
    ;(println (shortest-path (shortest-paths-per-cell available-paths)))
    (if-not (empty? available-paths)
      (->> (shortest-path (shortest-paths-per-cell available-paths)) last (take 2) vec))
    ))

(defn enemy-to-attack [unit cavern-map]
  (let [enemy (enemy-type unit)
        adjacent-cells (list-adjacent-cells [(:y unit) (:x unit)])
        cell-content (map #(get-in cavern-map %) adjacent-cells)]
    (->> (filter #(and (not= :free %) (not= :wall %)) cell-content)
         (filter #(= enemy (:type %)))
         first)))

(defn attack [attacker defender]
  (assoc! defender :hp (- (:hp defender) (:ap attacker))))

(defn dead? [unit]
  (>= 0 (:hp unit)))

(defn move-unit [unit [y x]]
  (assoc! unit :y y :x x))

(defn update-map-after-move [unit prev-loc cavern-map]
  (let [unit-pos [(:y unit) (:x unit)]]
    (assoc-in (assoc-in cavern-map prev-loc :free) unit-pos unit)))

(defn sort-units-by-reading-order [units]
  (sort-by :y units))

(defn play-round [units cavern-map]
  (loop [unit (first (sort-units-by-reading-order units))
         remaining-units (rest (sort-units-by-reading-order units))
         not-dead units
         cavern-map cavern-map]
    (println "Remaining" (count remaining-units))
    (if-not (nil? unit)
      (let [to-attack (enemy-to-attack unit cavern-map)]
        (println "Can attack" to-attack)
        (if-not (nil? to-attack)
          (let [to-attack (attack unit to-attack)
                remaining-not-dead (filter (complement dead?) remaining-units)
                not-dead-units (filter (complement dead?) not-dead)]
            (println "Attacked" (:id to-attack) "HP" (:hp to-attack))
            (attack unit to-attack)
            (if (dead? to-attack)
              (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units (update-in cavern-map [(:y to-attack) (:x to-attack)] :free))
              (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units cavern-map)
              ))
          (let [next-move (choose-next-move-destination unit not-dead cavern-map)
                current-loc [(:y unit) (:x unit)]]
            (println "Moving from" current-loc "To" next-move)
            (if-not (nil? next-move)
              (do
                (move-unit unit next-move)
                (recur (first remaining-units) (rest remaining-units) not-dead (update-map-after-move unit current-loc cavern-map)))
              (recur (first remaining-units) (rest remaining-units) not-dead cavern-map)))))
      cavern-map)))

(defn print-map [cavern-map]
  (doseq [row cavern-map]
    (println (apply str (map (fn [cell]
                               (cond
                                     (= :wall cell) "#"
                                     (= :free cell) "."
                                     (= :elf (:type cell)) "E"
                                     (= :goblin (:type cell)) "G"
                                     :else "X")) row)
                    ))))

(defn part-1 []
  (let [[cavern-map units] (parse-map-and-units "day15-example3.txt")]
    ;(choose-next-move-destination (nth units 0) units cavern-map)
    (->> (play-round units cavern-map)
         (play-round units)
         ;(play-round units)
         ;(play-round units)
         print-map)))