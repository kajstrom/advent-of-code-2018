(ns clj.day24
  (:require [clj.shared :refer :all]
            [clojure.string :as s]))

(defn parse-keywords [filt immunities-and-weaknesses]
  (let [immunities-str (first (filter #(= (.indexOf (s/trim %) filt) 0) immunities-and-weaknesses))]
    (if-not (nil? immunities-str)
      (map #(keyword (s/trim %)) (-> (subs immunities-str (count filt)) (s/split #", ")))
      [])))

(def id (atom 0))
(defn parse-group [side descr]
  (let [[units hp ap attack-type initiative] (->> (re-find #"([0-9]{1,4}) units each with ([0-9]{1,5}) hit points.*that does ([0-9]{1,5}) ([a-z]{1,}) damage at initiative ([0-9]{1,5})" descr) (drop 1))
        immunities-weaknesses (-> (s/split descr #"\(|\)") second (s/split #";"))]
    (swap! id inc)
    {:id @id
     :side side
     :units (parse-int units)
     :hp-per-unit (parse-int hp)
     :ap-per-unit (parse-int ap)
     :attack-type (keyword attack-type)
     :initiative (parse-int initiative)
     :weaknesses (parse-keywords "weak to " immunities-weaknesses)
     :immunities (parse-keywords "immune to" immunities-weaknesses)
     }))

(defn parse-groups [side file]
  (->> (split-lines-from-file file)
       (map #(parse-group side %))))

(defn effective-power [group]
  (* (:units group) (:ap-per-unit group)))

(defn deals-damage-to [group defender]
  (let [attack-type (:attack-type group)
        ep (effective-power group)]
    (cond
      (in? (:immunities defender) attack-type) 0
      (in? (:weaknesses defender) attack-type) (* 2 ep)
      :else ep
      )
    ))

(defn enemies-of [group all-groups]
  (let [enemy-type (if (= :immune (:side group)) :infection :immune)]
    (filter #(= enemy-type (:side %)) all-groups)))

(defn select-target-for [group available]
  (let [enemies (enemies-of group available)
        my-damage-to (partial deals-damage-to group)]
    (println (reverse (sort-by (juxt my-damage-to effective-power :initiative) enemies)))
    (first (reverse (sort-by (juxt my-damage-to effective-power :initiative) enemies)))))

(defn select-targets [groups]
  (let [sorted-groups (reverse (sort-by (juxt effective-power :initiative) groups))]
    (loop [selecting-group (first sorted-groups)
           sorted-groups (rest sorted-groups)
           available-targets groups
           selected-targets {}]
      (if (nil? selecting-group)
        selected-targets
        (let [target (select-target-for selecting-group available-targets)
              available-targets (filter #(not= (:id target) (:id %)) available-targets)]
          (println "Group" (:id selecting-group) "Selected target" (:id target) "Would deal damage" (deals-damage-to selecting-group target))
          (recur (first sorted-groups) (rest sorted-groups) available-targets (assoc selected-targets (:id selecting-group) (:id target))))))))

(defn part-1 []
  (reset! id 0)
  (let [immune (parse-groups :immune "day24-example-immune.txt")
        infection (parse-groups :infection "day24-example-infection.txt")]
    (select-targets (concat immune infection))))