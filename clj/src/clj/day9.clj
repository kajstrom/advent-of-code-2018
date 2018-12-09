(ns clj.day9
  (:require [clj.shared :refer []]))

(defn make-marble [value] (transient {:value value :next nil :prev nil}))

(defn circle-len [circle]
  (loop [marble circle
         len 0]
    (if-not (nil? marble)
      (recur (:next marble)
             (inc len))
      len)))

(defn marble-at-idx [circle idx]
  (loop [marble circle
         current-idx 0]
    (if (= idx current-idx)
      marble
      (recur (:next marble)
             (inc current-idx)))))

; For debugging purposes
(defn collect-values [circle]
  (loop [marble circle
         values []]
    (if-not (nil? (:next marble))
      (recur (:next marble) (conj values (:value marble)))
      (conj values (:value marble)))))

(defn move-forward [circle marble]
  (if (nil? (:next marble))
    circle
    (:next marble)))

(defn move-backwards [circle marble]
  (loop [marble marble
         steps 7]
    (do
      (if (= 0 steps)
        marble
        (if-not (nil? (:prev marble))
          (recur (:prev marble) (dec steps))
          (recur (marble-at-idx circle (dec (circle-len circle))) (dec steps)))))))

(defn remove-marble [marble]
  (let [prev (:prev marble)
        next (:next marble)]
    (if-not (nil? prev)
      (assoc! prev :next next))
    (if-not (nil? next)
      (assoc! next :prev prev))
    next))

(defn add-marble-next-to [marble to-add]
  (let [next (:next marble)]
    (assoc! to-add :next (:next marble))
    (assoc! to-add :prev marble)
    (if-not (nil? (:prev next))
      (assoc! next :prev to-add))
    (assoc! marble :next to-add)
    to-add))

(defn play-game [players marbles]
  (let [circle (make-marble 0)]
    (loop [current-player 0
           current-marble circle
           marbles marbles
           score (vec (repeat players 0))]
      (if-not (empty? marbles)
        (let [add-next-to (move-forward circle current-marble)
              marble-val (first marbles)]
          ;(when (= 0 (mod (count marbles) 71588)) (println (count marbles)))
          (if-not (and (not= 0 marble-val) (= 0 (mod marble-val 23)))
            (recur (mod (inc current-player) players)
                   (add-marble-next-to add-next-to (make-marble marble-val))
                   (rest marbles)
                   score)
            (let [marble-to-remove (move-backwards circle current-marble)
                  points (+ (:value marble-to-remove) marble-val)]
              (recur (mod (inc current-player) players)
                     (remove-marble marble-to-remove)
                     (rest marbles)
                     (update score current-player + points)))))
        score))))

(defn part-1 []
  (let [players 430
        marbles (range 1 (inc 71588))]
    (apply max (play-game players marbles))))

(defn part-2 []
  (let [players 430
        marbles (range 1 (inc (* 100 71588)))]
    (apply max (play-game players marbles))))

(defn time-results []
  (time
    (do
      (println "Part 1:")
      (time (part-1))
      (println "Part 2:")
      (time (part-2)))))