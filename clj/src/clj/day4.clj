(ns clj.day4
  (:require
    [clj.shared :refer [split-lines-from-file parse-int map-hashmap]]
    [clojure.string :as s]))

(defn parse-log-entry [entry]
  (let [splitted (s/split entry #"]")
        time (s/replace (first splitted) "[" "")
        entry-msg (last splitted)
        entry-map (hash-map :time time :orig-entry entry-msg :minute (-> (s/split time #":") last parse-int))]
    (cond
          (not= -1 (.indexOf entry-msg "wakes")) (assoc entry-map :event :wakeup)
          (not= -1 (.indexOf entry-msg "falls asleep")) (assoc entry-map :event :falls-asleep)
          (not= -1 (.indexOf entry-msg "#"))
            (assoc entry-map :event :shift-start :guard (-> (clojure.string/split entry-msg #"#") second (clojure.string/split #" ") first parse-int))
          :else entry-map)))

(defn load-log []
  (let [sorted-log (sort-by :time (map parse-log-entry (split-lines-from-file "day4.txt")))
        current-guard (atom nil)]
    (for [entry sorted-log]
      (if (= :shift-start (:event entry))
        (do
          (reset! current-guard (:guard entry))
          entry)
        (do
          (assoc entry :guard @current-guard))))))

(defn guard-sleep-minutes-from-log [log]
  (let [log-sleep-events (into {} (for [[k v] (group-by :guard log)] [k (filter #(not= :shift-start (:event %)) v)]))
        log-sleep-times (into {} (for [[k v] log-sleep-events] [k (map #(:minute %) v)]))
        log-sleep-ranges (into {} (for [[k v] log-sleep-times] [k (map #(apply range %) (partition 2 v))]))]
    (into {} (for [[k v] log-sleep-ranges] [k (flatten v)])))
  )

(defn guard-with-most-sleep [guard-sleep-minutes]
  (let [guard-sleep-totals (into {} (for [[k v] guard-sleep-minutes] [k (count v)]))]
    (key (apply max-key val guard-sleep-totals))))

(defn guard-asleep-most-minute [guard-sleep-minutes guard]
  (let [guard-minutes (get guard-sleep-minutes guard)
        sleep-minutes (group-by identity guard-minutes)
        minute-counts (into {} (for [[k v] sleep-minutes] [k (count v)]))
        ]
    (key (apply max-key val minute-counts))))

(defn part-1 [guard-sleep-log]
  (let [guard (guard-with-most-sleep guard-sleep-log)
        asleep-most-at (guard-asleep-most-minute guard-sleep-log guard)]
    (* guard asleep-most-at)))

(defn part-2 [guard-sleep-log]
  (let [guard-sleep-by-minutes (into {} (for [[k v] guard-sleep-log] [k (group-by identity v)]))
        guard-sleep-by-minutes-count (into {} (for [[k v] guard-sleep-by-minutes :when (not (empty? v))] [k (into {} (for [[k2 v2] v] [k2 (count v2)]))]))
        guards-most-sleep-on-same-minute (into {} (for [[k v] guard-sleep-by-minutes-count] [k (apply max (vals v))]))
        guard-with-most-sleep-on-same-minute (key (apply max-key val guards-most-sleep-on-same-minute))
        minutes-of-guard-wit-most (get guard-sleep-by-minutes-count guard-with-most-sleep-on-same-minute)
        minute (key (apply max-key val minutes-of-guard-wit-most))
        ]
    (* guard-with-most-sleep-on-same-minute minute)))

(defn time-results []
  (time
    (let [guard-sleep-log (guard-sleep-minutes-from-log (load-log))]
      (println "Part 1:")
      (time (part-1 guard-sleep-log))
      (println "Part 2:")
      (time (part-2 guard-sleep-log))
      (println "Total:"))))