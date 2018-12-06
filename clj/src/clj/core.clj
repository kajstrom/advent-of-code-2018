(ns clj.core
  (:gen-class)
  (:require [clj.day5 :as d5]
            [clj.day6 :as d6]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Day 5")
  (d5/time-results)
  (println "Day 6")
  (d6/time-results))
