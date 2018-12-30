(ns clj.day20
  (:require [clj.shared :refer :all]))

; On ( call another function to handle branching
; This other function will call shortest-path again

(declare shortest-path)

(defn load-input [file]
  (-> (split-lines-from-file file)
      first))

(defn branch [idx path]
  (loop [idx idx
         length-options []]
    (if (= \) (get path idx))
      (do
        ;(println "Chose from" length-options)
        (if (= \| (get path (dec idx)))
          [(inc idx) 0]
          [(inc idx) (apply max length-options)])
        )
      (let [[last-idx steps] (shortest-path idx path)]
        (recur last-idx (conj length-options steps))))))

;Returns [idx steps] Returns on | or $ or ) calls branch on (
(defn shortest-path [idx path]
  (loop [idx idx
         steps 0]
    ;(println idx steps (get path idx))
    (let [c (get path idx)]
      (case c
        nil (println "Index out of bounds" idx)
        \| [(inc idx) steps]
        \$ [idx steps]
        \) [idx steps]
        \( (let [[idx add-steps path-lengths] (branch (inc idx) path)]
             (recur idx (+ steps add-steps)))
        (recur (inc idx) (inc steps))))))

(defn old-part-1 []
  (let [path (load-input "day20.txt")]
    (second (shortest-path 1 path))))

; Part 1 solution can't handle part 2 needs in a sensible way.
; So to solve part two part 1 needs to be partially reimplemented.

(defn room-of [x y]
  {:x x :y y})

(defn east-of [{x :x y :y}]
  (room-of (dec x) y))

(defn west-of [{x :x y :y}]
  (room-of (inc x) y))

(defn north-of [{x :x y :y}]
  (room-of x (dec y)))

(defn south-of [{x :x y :y}]
  (room-of x (inc y)))

(def move-to-fn
  {
   \E east-of
   \W west-of
   \N north-of
   \S south-of
   })

(declare build-paths)

(defn path-branch [idx path start orig-steps]
  (loop [idx idx
         length-options []
         branches []]
    (if (= \) (get path idx))
      (do
        (if (= \| (get path (dec idx)))
          [(inc idx) 0 branches]
          [(inc idx) (apply max length-options) branches])
        )
      (let [[last-idx steps path] (build-paths idx path [] start orig-steps)]
        (recur last-idx (conj length-options steps) (conj branches path))))))

(def rooms-over1k (atom []))

(defn build-paths [idx path-str starting-path start orig-steps]
  (loop [idx idx
         steps orig-steps
         path starting-path
         room start]
    (let [c (get path-str idx)]
      (case c
        nil (println "Index out of bounds" idx)
        \| [(inc idx) (- steps orig-steps) path]
        \$ [idx (- steps orig-steps) path]
        \) [idx (- steps orig-steps) path]
        \( (let [[idx add-steps branch] (path-branch (inc idx) path-str room steps)]
             (recur idx (+ steps add-steps) (conj path branch) room))
        (let [next-room ((get move-to-fn c) room)]
          (if (>= (inc steps) 1000)
            (swap! rooms-over1k conj next-room))
          (recur (inc idx) (inc steps) (conj path next-room) next-room))))))

(defn part-1-and-2 []
  (let [path (load-input "day20.txt")
        start-room (room-of 0 0)
        [idx steps rooms] (build-paths 1 path [start-room] start-room 0)]
    (println "Part 1" steps)
    (println "Part 2" (count (distinct @rooms-over1k)))
    ))