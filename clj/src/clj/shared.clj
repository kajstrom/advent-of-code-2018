(ns clj.shared
  (:require [clojure.java.io :refer [resource]]))

(defn parse-int [str]
  (Integer/parseInt str))

;https://stackoverflow.com/questions/8056645/returning-duplicates-in-a-sequence
(defn dups [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(defn split-lines-from-file [file]
  (->> (resource file)
       slurp
       clojure.string/split-lines))

(defn in? [coll item] (some #(= item %) coll))

(def not-in? (complement in?))

(defn index-of [coll x] (.indexOf coll x))

(defn manhattan-distance [[a b c] [d e f]]
  (Math/abs (+ (Math/abs (- a d)) (Math/abs (- b e)) (if-not (nil? c) (Math/abs (- c f)) 0))))

(defn flatten-1 [coll]
  (mapcat identity coll))