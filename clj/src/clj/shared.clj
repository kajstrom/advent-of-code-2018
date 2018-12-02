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
