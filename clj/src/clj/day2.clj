(ns clj.day2
  (:require
    [clojure.java.io :refer [resource]]
    [clojure.string :as s]
    [clojure.data :refer [diff]]
    ))

(defn string-to-charmap
  [s]
  (apply hash-map (flatten (map (fn [[k v]] (list k (count v))) (group-by identity (seq s))))))

(defn id-checksum
  [file]
  (->> (resource file)
       slurp
       s/split-lines
       (map string-to-charmap)
       (map vals)
       (map (fn [char-counts] (filter #(> % 1) char-counts)))
       (map distinct)
       flatten
       (group-by identity)
       vals
       (map count)
       (apply *)
       ))

(defn part-1 []
  (id-checksum "day2.txt"))

(defn string-difference [s1 s2]
  (let [s1 (seq (char-array s1))
        s2 (seq (char-array s2))]
    (map-indexed (fn [idx char]
             (if (= char (nth s2 idx))
               char
               nil))
                 s1)))

(defn differs-by-one [s1 s2]
  (->> (string-difference s1 s2)
       (filter nil?)
       count
       (= 1)))

(defn ids-from-file [file]
  (->> (resource file)
       slurp
       s/split-lines))

(defn correct-ids [ids]
  (->> (reduce (fn [acc id]
                 (assoc acc id (filter #(differs-by-one id %) ids))
                 ) {} ids)
       (into {} (filter #(-> % val empty? not)))
       keys
       ))

(defn common-chars-in-ids [[id1 id2]]
  (->> (string-difference id1 id2)
       (filter #(not (nil? %)))
       (apply str)))

(defn part-2 []
  (-> (ids-from-file "day2.txt")
      correct-ids
      common-chars-in-ids))