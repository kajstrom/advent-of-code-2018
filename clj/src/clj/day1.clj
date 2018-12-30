(ns clj.day1
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as s]
            [clj.shared :refer :all]))

(defn load-changes [file]
  (->> (resource file)
      slurp
      clojure.string/split-lines
      (map parse-int)))

(defn apply-changes [changes]
  (apply + changes))

(defn first-freq-reached-twice [orig-changes]
  (loop [change (first orig-changes)
         changes (rest orig-changes)
         frequency 0
         freqs-seen []
         unchecked-frequencies []
         ]
    (if (nil? change)
      (do
        (let [freqs-seen (sort freqs-seen)
              seen-before (some #(if (< -1 (java.util.Collections/binarySearch freqs-seen %)) %) unchecked-frequencies)]
          (if-not (nil? seen-before)
            seen-before
            (recur (first orig-changes) (rest orig-changes) frequency (concat freqs-seen unchecked-frequencies) []))))
      (let [new-frequency (+ frequency change)]
        (do
          (recur (first changes) (rest changes) new-frequency freqs-seen (conj unchecked-frequencies new-frequency)))))))

(defn part-1 []
  (let [changes (load-changes "day1.txt")]
    (apply-changes changes)))

(defn part-2 []
  (let [changes (load-changes "day1.txt")]
    (first-freq-reached-twice changes)))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (println (time (part-1)))
      (println "Part 2")
      (println (time (part-2))))))