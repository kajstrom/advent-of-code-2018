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
    (transient {:id @id
                :side side
                :units (parse-int units)
                :hp-per-unit (parse-int hp)
                :ap-per-unit (parse-int ap)
                :attack-type (keyword attack-type)
                :initiative (parse-int initiative)
                :weaknesses (parse-keywords "weak to " immunities-weaknesses)
                :immunities (parse-keywords "immune to " immunities-weaknesses)
                })))

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
    ;(println (reverse (sort-by (juxt my-damage-to effective-power :initiative) enemies)))
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

(defn units-killed-in-attack [attacker defender]
  (let [damage (deals-damage-to attacker defender)
        hp-per-unit (:hp-per-unit defender)
        units-killed (/ (- damage (mod damage hp-per-unit)) hp-per-unit)]
    (if (> units-killed (:units defender)) (:units defender) units-killed)))

(defn remove-destroyed [groups]
  (filter #(>= (:units %) 1) groups))

(defn attack! [groups selected-targets]
  (let [sorted-groups (sort-by :initiative > groups)]
    (loop [attacking-group (first sorted-groups)
           remaining-groups (rest sorted-groups)
           all-groups sorted-groups]
      (if (nil? attacking-group)
        all-groups
        (let [defending-group-id (get selected-targets (:id attacking-group))
              defending-group (first (filter #(= defending-group-id (:id %)) all-groups))]
          (if (nil? defending-group-id)
            (do
              (println "No target for group" (:id attacking-group))
              (recur (first remaining-groups) (rest remaining-groups) all-groups)
              )
            (let [units-killed (units-killed-in-attack attacking-group defending-group)]
              (println "Group" (:id attacking-group) "Attacks" (:id defending-group) "Killing" units-killed)
              (assoc! defending-group :units (- (:units defending-group) units-killed))
              (let [remaining-alive-groups (remove-destroyed remaining-groups)
                    all-alive-groups (remove-destroyed all-groups)]
                (recur (first remaining-alive-groups) (rest remaining-alive-groups) all-alive-groups))))
          )))))

(defn immune-system-won [groups]
  (every? #(= :immune (:side %)) groups))

(defn infection-won [groups]
  (every? #(= :infection (:side %)) groups))

(defn fight-until-victory [groups]
  (loop [groups groups]
    (if (or (immune-system-won groups) (infection-won groups))
      groups
      (let [targets (select-targets groups)
            remaining-groups (attack! groups targets)]
        (recur remaining-groups)))))

(defn units-remaining [groups]
  (apply + (map :units groups)))

(defn part-1 []
  (reset! id 0)
  (let [immune (parse-groups :immune "day24-immune.txt")
        infection (parse-groups :infection "day24-infection.txt")
        groups (concat immune infection)]
    (doseq [group groups]
      (println "Immunities" (:immunities group))
      (println "Weaknesses" (:weaknesses group))
      )
    (let [remaining-groups (fight-until-victory groups)]
      (if (immune-system-won remaining-groups)
        (println "Immune system won")
        (println "Infection won"))
      (units-remaining remaining-groups))
    ))