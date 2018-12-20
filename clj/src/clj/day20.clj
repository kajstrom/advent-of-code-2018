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

(defn part-1 []
  (let [path (load-input "day20.txt")]
    (second (shortest-path 1 path))))

(defn part-2 []
  (let [path (load-input "day20.txt")]
    (second (shortest-path 1 path))))