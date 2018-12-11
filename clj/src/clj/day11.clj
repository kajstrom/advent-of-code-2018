(ns clj.day11)

(def serial-number 6878)

(defn cell-fuel-level [serial-number [x y]]
  (let [rack-id (+ x 10)
        power-level (* rack-id (+ serial-number  (* rack-id y)))]
    (- (Character/digit (nth (reverse (str power-level)) 2 \0) 10) 5)))

(defn make-grid [serial-number]
  (vec (for [y (range 0 301)]
         (vec (for [x (range 0 301)]
                (if (and (> y 0) (> x 0))
                  (cell-fuel-level serial-number [x y])))))))

(defn calculate-total-power-of [grid [x y]]
  (let [x-axis (range x (+ 3 x))
        y-axis (range y (+ 3 y))
        values (transient [])]
    (doseq [x x-axis]
      (doseq [y y-axis]
        (conj! values (get-in grid [y x]))))
    (apply + (persistent! values))))

(defn calculate-total-powers [grid]
  (let [powers (transient [])]
    (doseq [y (range 1 298)]
      (doseq [x (range 1 298)]
        (conj! powers [(str x "," y) (calculate-total-power-of grid [x y])])
        ))
    (persistent! powers)))

(defn calculate-all-total-powers-from [grid [top-x top-y]]
  (let [x-axis (range top-x 301)
        sums (transient [])]
    (doseq [to-x x-axis]
      (let [y-axis (range top-y (mod (+ top-y (inc (- to-x top-x))) 301))
            values (transient [])]
        (doseq [x (range top-x (inc to-x))]
          (doseq [y y-axis]
            (conj! values (get-in grid [y x]))))
        (conj! sums [(str top-x "," top-y "," (count y-axis)) (apply + (persistent! values))])))
    (reduce (fn [carry powers]
              (if (> (last carry) (last powers))
                carry
                powers)) (persistent! sums))))

(defn calculate-all-total-powers [grid]
  (let [powers (transient [])]
    (doseq [y (range 1 301)]
      (println y)
      (doseq [x (range 1 301)]
        (conj! powers (future (calculate-all-total-powers-from grid [x y])))
        ))
    (map deref (persistent! powers))
    ))

(defn part-1 []
  (let [grid (make-grid serial-number)
        powers (calculate-total-powers grid)]
    (reduce (fn [carry powers]
              (if (> (last carry) (last powers))
                carry
                powers)) powers)
    ))

(defn part-2 []
  (let [grid (make-grid serial-number)
        powers (calculate-all-total-powers grid)]
    (reduce (fn [carry powers] (if (> (last carry) (last powers)) carry powers)) powers)
    ))