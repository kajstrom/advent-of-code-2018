(ns clj.day9
  (:require [clj.shared :refer []]))

(defn make-marble [value] (transient {:value value :next nil :prev nil}))

; For debugging purposes
; Doesn't work with the circular doubly linked list!
(defn collect-values [circle]
  (loop [marble circle
         values []]
    (if-not (nil? (:next marble))
      (recur (:next marble) (conj values (:value marble)))
      (conj values (:value marble)))))

(defn move-forward [marble]
  (if (nil? (:next marble))
    marble
    (:next marble)))

(defn move-backwards [marble]
  (loop [marble marble
         steps 7]
    (do
      (if (= 0 steps)
        marble
        (if-not (nil? (:prev marble))
          (recur (:prev marble) (dec steps))
          (println (:value marble)))))))

(defn remove-marble [marble]
  (let [prev (:prev marble)
        next (:next marble)]
    (assoc! prev :next next)
    (assoc! next :prev prev)
    next))

(defn add-marble-next-to [marble to-add]
  ;(println marble)
  (let [next (:next marble)]
    (if-not (nil? next)
      (assoc! to-add :next next)
      (assoc! to-add :next marble))
    (assoc! to-add :prev marble)
    (if-not (nil? (:prev next))
      (assoc! next :prev to-add)
      (assoc! marble :prev to-add))
    (assoc! marble :next to-add)
    to-add))

(defn play-game [players marbles]
  (let []
    (loop [current-player 0
           current-marble (make-marble 0)
           marbles marbles
           score (vec (repeat players 0))]
      (if-not (empty? marbles)
        (let [add-next-to (move-forward current-marble)
              marble-val (first marbles)]
          ;(println marble-val)
          ;(when (= 0 (mod (count marbles) 71588)) (println (count marbles)))
          (if-not (and (not= 0 marble-val) (= 0 (mod marble-val 23)))
            (recur (mod (inc current-player) players)
                   (add-marble-next-to add-next-to (make-marble marble-val))
                   (rest marbles)
                   score)
            (let [marble-to-remove (move-backwards current-marble)
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