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
            (every? #(let [[p-y p-x p-c] %]
                       ;(println p-y p-x y x (not (<= p-c c)))
                     (if (and (= p-y y) (= p-x x))
                       (not (<= p-c c))
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
      (if-not (> counter (* 100000 (count cavern-map)))
        (if-not (= idx (count path))
          (let [counter (inc counter)
                current (get path idx)
                adjacent-cells (find-free-cells-next-to (take 2 current) cavern-map)
                adjacent-with-count (map #(conj % counter) adjacent-cells)
                cells-to-visit (filter-equal-or-less-cells adjacent-with-count path)
                ]
            ;(println current)
            (if (empty? cells-to-visit)
              (recur counter path (inc idx))
              (if (some #(= orig (vec (take 2 %))) cells-to-visit)
                (do
                  ;(println "Found path to" orig)
                  (conj path (conj orig counter)))
                (recur counter (apply conj path cells-to-visit) (inc idx))))))
        (do
          ;(println "End reached, path not found")
          nil)))))

(defn available-in-range-paths [unit units cavern-map]
  (let [target-type (enemy-type unit)
        targets (filter #(= target-type (:type %))  units)
        cells-in-range-of-targets (flatten-1 (map #(find-free-cells-next-to [(:y %) (:x %)] cavern-map) targets))
        cells-next-to-unit (find-free-cells-next-to [(:y unit) (:x unit)] cavern-map)]
    ;(println cells-in-range-of-targets)
    (->> (map (fn [cell]
                (pmap #(shortest-path-to cell % cavern-map) cells-in-range-of-targets)
                ) cells-next-to-unit)
         (map (fn [cell-paths]
                (filter #(not= nil %) cell-paths)))
         (filter #(not (empty? %))))
    ))

(defn path-length [path]
  (loop [current (last path)
         remaining (drop-last path)
         actual-path [current]]
    (if (nil? current)
      (do
        ;(println actual-path)
        (count actual-path))
      (let [neighbor (first (filter #(= 1 (manhattan-distance (vec (take 2 current)) (vec (take 2 %)))) remaining))
            ]
        (if (nil? neighbor)
          (recur nil remaining actual-path)
          (recur neighbor (filter #(< (last %) (last neighbor)) remaining) (conj actual-path neighbor)))))))

(defn shortest-paths-per-cell [paths-cell]
  (->> (map (fn [paths]
             (let [shortest-path-length (apply min (pmap path-length paths))]
               (filter #(= shortest-path-length (-> % path-length)) paths)))
           paths-cell)
      (map first)))

(defn shortest-path [paths]
  ;(println paths)
  (let [shortest-length (apply min (pmap path-length paths))]
    (println shortest-length)
    (->> (filter #(= shortest-length (-> % path-length)) paths)
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
        cell-content (map #(get-in cavern-map %) adjacent-cells)
        possible-enemies (->> (filter #(and (not= :free %) (not= :wall %)) cell-content)
             (filter #(= enemy (:type %))))
        ]
    (if-not (empty? possible-enemies)
      (let [min-hp (apply min (map :hp possible-enemies))]
        (first (filter #(= min-hp (:hp %)) possible-enemies))))))

(defn attack [attacker defender]
  (assoc! defender :hp (- (:hp defender) (:ap attacker))))

(defn dead? [unit]
  (>= 0 (:hp unit)))

(def not-dead? (complement dead?))

(defn move-unit [unit [y x]]
  (assoc! unit :y y :x x))

(defn update-map-after-move [unit prev-loc cavern-map]
  (let [unit-pos [(:y unit) (:x unit)]]
    (assoc-in (assoc-in cavern-map prev-loc :free) unit-pos unit)))

(defn sort-units-by-reading-order [units]
  (sort-by (juxt :y :x) units))


(defn play-round [units cavern-map]
  (loop [unit (first units)
         remaining-units (rest units)
         not-dead units
         cavern-map cavern-map
         latest-was-a-death false]
    (println "Remaining" (count remaining-units))
    (if-not (nil? unit)
      (let [to-attack (enemy-to-attack unit cavern-map)]
        (println "Can attack" to-attack)
        (if-not (nil? to-attack)
          (let [to-attack (attack unit to-attack)
                remaining-not-dead (filter not-dead? remaining-units)
                not-dead-units (filter not-dead? not-dead)]
            (println "Attacked" (:id to-attack) "HP" (:hp to-attack))
            (if (dead? to-attack)
              (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units (assoc-in cavern-map [(:y to-attack) (:x to-attack)] :free) true)
              (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units cavern-map false)
              ))
          (let [next-move (choose-next-move-destination unit not-dead cavern-map)
                current-loc [(:y unit) (:x unit)]]
            (println "Moving from" current-loc "To" next-move)
            (if-not (nil? next-move)
              (do
                (move-unit unit next-move)
                (let [to-attack (enemy-to-attack unit cavern-map)]
                  (if-not (nil? to-attack)
                    (let [to-attack (attack unit to-attack)
                          remaining-not-dead (filter not-dead? remaining-units)
                          not-dead-units (filter not-dead? not-dead)]
                      (println "Attacked" (:id to-attack) "HP" (:hp to-attack))
                      (if (dead? to-attack)
                        (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units (assoc-in (update-map-after-move unit current-loc cavern-map) [(:y to-attack) (:x to-attack)] :free) true)
                        (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units (update-map-after-move unit current-loc cavern-map) false)
                        ))
                    (recur (first remaining-units) (rest remaining-units) not-dead (update-map-after-move unit current-loc cavern-map) false)
                    ))
                )
              (recur (first remaining-units) (rest remaining-units) not-dead cavern-map false)))))
      [cavern-map latest-was-a-death])))

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

(defn wins-type [type units]
  (let [enemy (if (= type :elf) :goblin :elf)]
    (every? dead? (filter #(= enemy (:type %)) units))))

(def elves-won (partial wins-type :elf))
(def goblins-won (partial wins-type :goblin))

(defn hp-left [units]
  (apply + (map :hp (filter not-dead? units))))

(defn print-hp-left [units]
  (doseq [unit (filter not-dead? units)]
    (let [type (if (= :elf (:type unit)) "E" "G")]
      (println type "HP" (:hp unit) "Y,X" (:y unit) (:x unit)))))

(defn play-until-victory [units cavern-map]
  (loop [cavern-map cavern-map
         units units
         rounds 0
         latest-was-a-death false]
    (println "Round" rounds)
    (print-map cavern-map)
    (print-hp-left units)
    (if-not (or (elves-won units) (goblins-won units))
      (let [[cavern-map latest-was-a-death] (play-round (sort-units-by-reading-order units) cavern-map)]
        (recur cavern-map (sort-units-by-reading-order (filter not-dead? units)) (inc rounds) latest-was-a-death))
      (let [rounds (if latest-was-a-death rounds (dec rounds))
            score (* rounds (hp-left units))]
        (println "Someone won! Rounds" rounds "HP left" (hp-left units) "Outcome" score)
        score))))

(defn part-1 []
  (let [[cavern-map units] (parse-map-and-units "day15.txt")]
    ;(choose-next-move-destination (nth units 0) units cavern-map)
    (play-until-victory units cavern-map)))