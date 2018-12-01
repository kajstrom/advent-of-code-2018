(ns clj.day1
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as s]
            [clj.shared :refer [parse-int]]))

(defn load-changes [file]
  (->> (resource file)
      slurp
      clojure.string/split-lines
      (map parse-int)))

(defn apply-changes [changes]
  (apply + changes))

(defn freq-seen-before [freq freqs-seen]
  (not (nil? (some #(= freq %) freqs-seen))))

(defn first-freq-reached-twice [changes frequency freqs-seen]
  (let [changes-left (atom changes)
        frequency (atom frequency)
        change (atom 0)
        freqs-seen (atom freqs-seen)]
    (while (and (not (empty? @changes-left)) (not (freq-seen-before @frequency @freqs-seen)))
      (do
        (swap! freqs-seen conj @frequency)
        (reset! change (first @changes-left))
        (reset! changes-left (rest @changes-left))
        (reset! frequency (+ @frequency @change))
        ))
    (if (freq-seen-before @frequency @freqs-seen)
      @frequency
      (do
        (println "Pass completed")
        (first-freq-reached-twice changes @frequency @freqs-seen)))))