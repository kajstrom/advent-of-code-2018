(ns clj.day16
  (:require [clj.shared :refer :all]
            [clojure.string :as s]
            [clojure.repl :as r]))

(defn parse-space-list [list]
  (->> (s/split list #" ")
       (map parse-int)
       vec))

(defn parse-comma-list [list]
  (->> (s/split list #", ")
      (map parse-int)
       vec))

(defn parse-cases [file]
  (->> (split-lines-from-file file)
      (partition 3 4)
       (map (fn [[before opcall after]]
              (let [before (subs before 9 19)
                    after (subs after 9 19)]
                [(parse-comma-list before) (parse-space-list opcall) (parse-comma-list after)])))))

(defn addr [register a b c]
  (assoc register c (+ (get register a) (get register b))))

(defn addi [register a b c]
  (assoc register c (+ (get register a) b)))

(defn mulr [register a b c]
  (assoc register c (* (get register a) (get register b))))

(defn muli [register a b c]
  (assoc register c (* (get register a) b)))

(defn banr [register a b c]
  (assoc register c (bit-and (get register a) (get register b))))

(defn bani [register a b c]
  (assoc register c (bit-and (get register a) b)))

(defn borr [register a b c]
  (assoc register c (bit-or (get register a) (get register b))))

(defn bori [register a b c]
  (assoc register c (bit-or (get register a) b)))

(defn setr [register a b c]
  (assoc register c (get register a)))

(defn seti [register a b c]
  (assoc register c a))

(defn gtir [register a b c]
  (assoc register c (if (> a (get register b)) 1 0)))

(defn gtri [register a b c]
  (assoc register c (if (> (get register a) b ) 1 0)))

(defn gtrr [register a b c]
  (assoc register c (if (> (get register a) (get register b)) 1 0)))

(defn eqir [register a b c]
  (assoc register c (if (= a (get register b)) 1 0)))

(defn eqri [register a b c]
  (assoc register c (if (= (get register a) b) 1 0)))

(defn eqrr [register a b c]
  (assoc register c (if (= (get register a) (get register b)) 1 0)))

(def opcodes
  [
   addr addi
   mulr muli
   banr bani
   borr bori
   setr seti
   gtir gtri gtrr
   eqir eqri eqrr
   ])

(defn behaves-like-opcodes [opcode register-before register-after]
  (let [[o a b c] opcode]
    (->> (reduce (fn [carry opcode-fn]
                   (if (= register-after (opcode-fn register-before a b c))
                     (conj carry opcode-fn)
                     carry)) [] opcodes)
         doall
         (map str)
         (map r/demunge))))

(defn part-1 []
  (let [cases (parse-cases "day16-cases.txt")
        behaviors (for [[before opcode after] cases]
                    [(first opcode) (behaves-like-opcodes opcode before after)])]
    (count (filter #(>= (last %) 3) (map (fn [[o fns]] [o (-> fns count)]) behaviors)))
    )
  )