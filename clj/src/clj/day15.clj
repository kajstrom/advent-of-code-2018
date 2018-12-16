(ns clj.day15
  (:require [clj.shared :refer :all]))

(def logging-enabled (atom true))
(defn log-message [m & args]
  (if @logging-enabled
    (apply println (conj args m ))))

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

(defn find-free-cells-of-orig-next-to [orig [y x] cavern-map]
  (let [adjacent-cells (list-adjacent-cells [y x])]
    (filter #(not= nil %) (map (fn [coords]
                                 (let [cell (get-in cavern-map coords)]
                                   (if (= orig coords)
                                     coords
                                     (if (= :free cell)
                                       coords))
                                   )) adjacent-cells))))

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
                adjacent-cells (find-free-cells-of-orig-next-to orig (take 2 current) cavern-map)
                adjacent-with-count (map #(conj % counter) adjacent-cells)
                cells-to-visit (filter-equal-or-less-cells adjacent-with-count path)
                ]
            ;(println current adjacent-cells)
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
        targets (filter #(= target-type (:type %)) units)
        cells-in-range-of-targets (flatten-1 (map #(find-free-cells-next-to [(:y %) (:x %)] cavern-map) targets))
        cells-next-to-unit (find-free-cells-next-to [(:y unit) (:x unit)] cavern-map)]
    ;(println cells-in-range-of-targets)
    (->> (pmap #(shortest-path-to [(:y unit) (:x unit)] % cavern-map) cells-in-range-of-targets)
         (filter #(not= nil %))
         ;(filter #(not (empty? %)))
         )
    ))

(defn valid-path-from [path]
  (loop [current (last path)
         remaining (drop-last path)
         actual-path [current]]
    (if (nil? current)
      (do
        ;(println actual-path)
        ;(count actual-path)
        (vec (reverse actual-path))
        )
      (let [neighbor (first (filter #(in? (list-adjacent-cells (vec (take 2 current))) (vec (take 2 %))) remaining))
            ]
        (if (nil? neighbor)
          (recur nil remaining actual-path)
          (recur neighbor (filter #(< (last %) (last neighbor)) remaining) (conj actual-path neighbor)))))))

(defn path-length [path]
  (loop [current (last path)
         remaining (drop-last path)
         actual-path [current]]
    (if (nil? current)
      (do
        ;(println actual-path)
        (count actual-path))
      (let [neighbor (first (filter #(in? (list-adjacent-cells (vec (take 2 current))) (vec (take 2 %))) remaining))
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

(defn shortest-paths [paths]
  ;(println paths)
  (let [shortest-length (apply min (pmap path-length paths))]
    (println "Shortest path length" shortest-length)
    (filter #(= shortest-length (-> % path-length)) paths)

    ))

(defn choose-next-move-destination [unit units cavern-map]
  (let [cells-next-to-unit (find-free-cells-next-to [(:y unit) (:x unit)] cavern-map)
        available-paths (available-in-range-paths unit units cavern-map)
        valid-available-paths (pmap valid-path-from available-paths)
        ]
    ;(println "Available path cnt" available-paths)
    (if-not (empty? valid-available-paths)
      (let [shortest (sort-by (juxt first first last) (shortest-paths valid-available-paths))]
        (println "Shortest paths" shortest)
        (->> shortest first drop-last last (take 2) vec))
      )
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

(defn has-alive-enemies [unit units]
  (let [type (enemy-type unit)]
    (not (empty? (filter #(and (not-dead? %) (= type (:type %))) units)))))

(defn play-round [units cavern-map]
  (loop [unit (first units)
         remaining-units (rest units)
         not-dead units
         cavern-map cavern-map]
    (log-message "Remaining" (count remaining-units))
    (log-message "Active unit" (:id unit))
    (if-not (nil? unit)
      (if (has-alive-enemies unit units)
        (let [to-attack (enemy-to-attack unit cavern-map)]
          ;(println "Can attack" to-attack)
          (if-not (nil? to-attack)
            (let [to-attack (attack unit to-attack)
                  remaining-not-dead (filter not-dead? remaining-units)
                  not-dead-units (filter not-dead? not-dead)]
              (log-message "Unit" (:id unit) "attacked" (:id to-attack) "HP" (:hp to-attack))
              (log-message (:id unit) remaining-not-dead)
              (if (dead? to-attack)
                (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units (assoc-in cavern-map [(:y to-attack) (:x to-attack)] :free))
                (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units cavern-map)
                ))
            (let [next-move (choose-next-move-destination unit not-dead cavern-map)
                  current-loc [(:y unit) (:x unit)]]
              ;(println "Moving from" current-loc "To" next-move)
              (if-not (nil? next-move)
                (do
                  (move-unit unit next-move)
                  (let [to-attack (enemy-to-attack unit cavern-map)]
                    (if-not (nil? to-attack)
                      (let [to-attack (attack unit to-attack)
                            remaining-not-dead (filter not-dead? remaining-units)
                            not-dead-units (filter not-dead? not-dead)]
                        ;(println "Attacked" (:id to-attack) "HP" (:hp to-attack))
                        (log-message (:id unit) remaining-not-dead)
                        (if (dead? to-attack)
                          (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units (assoc-in (update-map-after-move unit current-loc cavern-map) [(:y to-attack) (:x to-attack)] :free))
                          (recur (first remaining-not-dead) (rest remaining-not-dead) not-dead-units (update-map-after-move unit current-loc cavern-map))
                          ))
                      (recur (first remaining-units) (rest remaining-units) not-dead (update-map-after-move unit current-loc cavern-map))
                      ))
                  )
                (recur (first remaining-units) (rest remaining-units) not-dead cavern-map)))))
        [cavern-map :victory-turns-left
         ])
      [cavern-map (if (has-alive-enemies (first units) units) :all-turns-taken :victory-all-turns-taken ) ])))

(defn print-map [cavern-map]
  (doseq [row cavern-map]
    (log-message (apply str (map (fn [cell]
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
      (log-message type "(" (:id unit) ")" "HP" (:hp unit) "Y,X" (:y unit) (:x unit)))))

(defn report-victory [rounds hp last-turn-type]
  (let [score (* rounds hp)]
    (log-message "Last was a" last-turn-type)
    (log-message "Someone won! Rounds" rounds "HP left" hp "Outcome" score)
    score))

(defn play-until-victory [units cavern-map]
  (loop [cavern-map cavern-map
         units units
         rounds 1]
    (log-message "After" rounds)
    (print-map cavern-map)
    (print-hp-left units)
    (let [[cavern-map latest-turn-end-type] (play-round (sort-units-by-reading-order units) cavern-map)
          ]
      ;(println rounds)
      (case latest-turn-end-type
        :all-turns-taken (recur cavern-map (sort-units-by-reading-order (filter not-dead? units)) (inc rounds))
        :victory-turns-left (report-victory (dec rounds) (hp-left units) latest-turn-end-type)
        :victory-all-turns-taken (report-victory rounds (hp-left units) latest-turn-end-type))
      )))

(defn part-1 []
  (reset! logging-enabled true)
  (let [[cavern-map units] (parse-map-and-units "day15.txt")]
    ;(choose-next-move-destination (nth units 0) units cavern-map)
    (play-until-victory units cavern-map)))