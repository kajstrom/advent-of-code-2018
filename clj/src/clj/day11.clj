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

(defn part-1 []
  (let [grid (make-grid serial-number)
        powers (calculate-total-powers grid)]
    (reduce (fn [carry powers]
              (if (> (last carry) (last powers))
                carry
                powers)) powers)
    ))