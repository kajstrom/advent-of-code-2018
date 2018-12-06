(ns clj.day5
  (:require [clojure.string :as s]
            [clojure.java.io :refer [resource]]))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn polymer-react-regex []
  (let [chars (char-range \a \z)]
    (re-pattern (apply str (for [c chars] (let [cUp (Character/toUpperCase c)]
                                            (str c cUp \| cUp c \|)))))))

(defn react-all [polymers]
  (let [regex (polymer-react-regex)]
    (loop [polymers polymers
           length 0]
      (if-not (= length (count polymers))
        (recur (clojure.string/replace polymers regex "") (count polymers))
        polymers))))

(defn part-1 []
  (count (react-all (-> (resource "day5.txt") slurp))))

(defn char-lower-and-upper-regex [c]
  (re-pattern (str "[" c (Character/toUpperCase c) "]")))

(defn strip-polymers [to-strip polymers]
  (s/replace polymers (char-lower-and-upper-regex to-strip) ""))

(defn stripped-polymers-by-char [polymers]
  (let [chars (char-range \a \z)]
    (into {} (for [c chars] [c (strip-polymers c polymers)]))))

(defn react-polymers-by-chars [polymers-by-char]
  (into {} (for [[k v] polymers-by-char] (do
                                           [k (count (react-all v))]))))

(defn part-2 []
  (let [polymers (-> (resource "day5.txt") slurp)
        polymers-by-stripped-char (stripped-polymers-by-char polymers)
        reacted-polymers-length-by-char(react-polymers-by-chars polymers-by-stripped-char)]
    (apply min-key val reacted-polymers-length-by-char)))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1))
      (println "Part 2:")
      (time (part-2)))))