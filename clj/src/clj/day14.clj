(ns clj.day14
  (:require [clj.shared :refer :all]))

(defn str-ints-to-ints [string]
  (vec (map #(Character/digit % 10) (seq (char-array string)))))

(defn recipes-from-string [string]
  (str-ints-to-ints string))

(def elf-id-counter (atom 0))

(defn make-elf [pos]
  (let [id @elf-id-counter]
    (swap! elf-id-counter inc)
    (transient {:pos pos :id id})))

(defn create-new-recipes [elves recipes]
  (let [positions (map #(get recipes %) (map :pos elves))
        digits (apply + positions)
        digits-seq (str-ints-to-ints (str digits))]
    digits-seq))

(defn move-elves [elves recipes]
  (let [res-cnt (count recipes)]
    (doseq [elf elves] (let [moves (+ 1 (get recipes (:pos elf)))
                             next-pos (mod (+ moves (:pos elf)) res-cnt)
                             elves-except-this-one (filter #(not= (:id elf) (:id %)) elves)
                             next-pos-taken? (not (empty? (filter #(= next-pos (:pos %)) elves-except-this-one)))]
                         (if-not next-pos-taken?
                           (assoc! elf :pos next-pos)
                           (if (>= (inc next-pos) res-cnt)
                             (assoc! elf :pos 0)
                             (assoc! elf :pos (inc next-pos))))
                       ))))

; Debugging
(defn print-elf-position [elves]
  (doseq [elf elves]
    (println "Elf" (:id elf) "Pos" (:pos elf))))

(defn create-recipes-times [recipe-cnt elves recipes]
  (loop [recipes-left recipe-cnt
         curr-recipes recipes]
    ;(when (= 0 (mod recipes-left 100000)) (println "Recipes left" recipes-left))
    (if-not (= recipes-left 0)
      (let [new-recipes (apply conj curr-recipes (create-new-recipes elves curr-recipes))]
        (move-elves elves new-recipes)
        (recur (dec recipes-left) new-recipes))
      curr-recipes)))

(defn part-1 []
  (reset! elf-id-counter 0)
  (let [
        recipes (recipes-from-string "37")
        elves [(make-elf 0) (make-elf 1)]
        final-recipes (create-recipes-times 640441 elves recipes)]
    (apply str (take 10 (drop 640441 final-recipes))))
  )

(defn create-recipe-until-score-found [score elves recipes]
  (loop [recipes-left 1000000
         curr-recipes recipes]
    (if-not (= recipes-left 0)
      (let [new-recipes (apply conj curr-recipes (create-new-recipes elves curr-recipes))]
        (move-elves elves new-recipes)
        (recur (dec recipes-left) new-recipes))
      (let [index (.indexOf (apply str curr-recipes) score)]
        (if (not= -1 index)
          index
          (recur 1000000 curr-recipes))))))

(defn part-2 []
  (reset! elf-id-counter 0)
  (let [recipes (recipes-from-string "37")
        elves [(make-elf 0) (make-elf 1)]]
    (create-recipe-until-score-found "640441" elves recipes))
  )

(defn time-results []
  (time (do
          (println "Part 1")
          (time (part-1))
          (println "Part 2")
          (time (part-2)))))