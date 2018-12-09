(ns clj.day9
  (:require [clj.shared :refer []]))

(defn next-idx [circle current-idx]
  (let [next (+ 2 current-idx)
        circle-cnt (count circle)]
    (cond
      (<= next circle-cnt) next
      (> next circle-cnt) (- next circle-cnt))))

(defn place-marble [circle idx marble]
  (if (= (count circle) idx)
    (conj circle marble)
    (let [[head tail] (split-at idx circle)]
      (vec (concat head [marble] tail)))))

(defn remove-marble [circle idx]
  (let [[head tail] (split-at idx circle)]
    (vec (concat head (rest tail)))))

(defn marble-idx-to-remove [circle current-idx]
  (let [idx-to-remove (- current-idx 7)]
    (cond
      (neg? idx-to-remove) (+ (count circle) idx-to-remove)
      :else idx-to-remove)))

(defn play-game [players marbles]
  (let []
    (loop [current-player 0
           circle [0]
           current-idx 0
           marbles marbles
           score (vec (repeat players 0))]
      (if-not (empty? marbles)
        (let [idx (next-idx circle current-idx)
              marble (first marbles)]
          ;(println current-player)
          (when (= 0 current-player) (println (count marbles)))
          (if-not (and (not= 0 marble) (= 0 (mod marble 23)))
            (recur (mod (inc current-player) players)
                   (place-marble circle idx marble)
                   idx
                   (rest marbles)
                   score)
            (let [remove-idx (marble-idx-to-remove circle current-idx)
                  removed-marble (nth circle remove-idx)
                  points (+ removed-marble marble)]
              ;(println score current-player)
              ;(println (update score current-player inc))
              ;(println "ads")
              (recur (mod (inc current-player) players)
                     (remove-marble circle remove-idx)
                     remove-idx
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