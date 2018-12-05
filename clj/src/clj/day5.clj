(ns clj.day5
  (:require [clojure.string :as s]
            [clojure.java.io :refer [resource]]))

(defn same-char? [char1 char2]
  (let [char1 (s/upper-case char1)
        char2 (s/upper-case char2)]
    (= char1 char2)))

(defn react? [char1 char2]
  (let [both-uppercase (and (Character/isUpperCase char1) (Character/isUpperCase char2))
        both-lowercase (and (Character/isLowerCase char1) (Character/isLowerCase char2))
        polarities-match (or both-uppercase both-lowercase)
        are-same-char (same-char? char1 char2)]
    (and (not polarities-match) are-same-char)))

; This should have a better solution. `for`?
(defn react-polymers [polymers]
  (if (= 1 (count polymers))
    polymers
    (let [length (count polymers)
          position (atom 0)
          next-pos (atom 1)
          reacted-polymers (atom [])]
      (while (> length @next-pos)
        (if (react? (get polymers @position) (get polymers @next-pos))
          (do
            (if (>= (+ @next-pos 2) length)
              (reset! reacted-polymers (apply conj @reacted-polymers (seq (char-array (subs polymers (+ @position 2) )))))
              )
            (swap! position + 2)
            (swap! next-pos + 2)
            )
          (do
            (swap! reacted-polymers conj (get polymers @position))
            (swap! position inc)
            (swap! next-pos inc)
            (if (= @next-pos length)
              (swap! reacted-polymers conj (last polymers))))))
      (apply str @reacted-polymers))))

(defn react-all [polymers]
  (let [polymers2 (atom polymers)
        length (atom 0)]
    (while (not= @length (count @polymers2))
      (do
        (reset! length (count @polymers2))
        (reset! polymers2 (react-polymers @polymers2))))
    @polymers2))

(defn part-1 []
  (count (react-all (-> (resource "day5.txt") slurp))))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1)))))