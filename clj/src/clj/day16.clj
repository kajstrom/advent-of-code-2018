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

(defn parse-test-program [file]
  (->> (split-lines-from-file file)
       (map parse-space-list)))

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
                     (update carry 0 conj opcode-fn )                 ;(-> (str opcode-fn) r/demunge)
                     (update carry 1 conj opcode-fn)
                     )
                   ) [[] []] opcodes)
         )))

(defn part-1 []
  (let [cases (parse-cases "day16-cases.txt")
        behaviors (for [[before opcode after] cases]
                    [(first opcode) (behaves-like-opcodes opcode before after)])]
    (count (filter #(>= (last %) 3) (map (fn [[o fns]] [o (-> fns first count)]) behaviors)))
    )
  )

(defn fns-and-counts [opcode-fns]
  (vector (first opcode-fns) (count (second opcode-fns))))

(defn group-passed-fns-by-opcode [cases-by-opcode]
  (into {} (for [[k v] cases-by-opcode] [k (let [stats (map last v)
                                                 passes (map first stats)
                                                 fails (distinct (map last stats))
                                                 fns-by-name (group-by identity (flatten (filter #(not-in? fails %) passes)))]
                                             (for [fns fns-by-name] (fns-and-counts fns))
                                             )])))

(defn remove-fn-from-remaining [fn-name remaining]
  (map (fn [[op-code fns]]
         [op-code (filter #(not= fn-name (first %)) fns)])  remaining))

(defn figure-opcodes [opcodes-and-fns]
  (loop [found-out []
         remaining opcodes-and-fns]
    (if (empty? remaining)
      found-out
      (let [only-one-op-code (first (filter #(= 1 (count (last %))) remaining))
            fn-name (-> (last only-one-op-code) first first)
            remaining-sans-fn (remove-fn-from-remaining fn-name remaining)]
        (recur (conj found-out [(first only-one-op-code) fn-name])
               (filter #(not (empty? (last %))) remaining-sans-fn)
               )))))

(defn execute-test-program [opcode-map test-prog]
  (loop [test-prog test-prog
         registers [0 0 0 0]]
    (if (empty? test-prog)
      registers
      (let [next-call (first test-prog)
            [opcode a b c] next-call
            opcode-fn (get opcode-map opcode)]
        (recur (rest test-prog) (opcode-fn registers a b c))))))

(defn part-2 []
  (let [cases (parse-cases "day16-cases.txt")
        behaviors (for [[before opcode after] cases]
                    [(first opcode) (behaves-like-opcodes opcode before after)])
        cases-by-opcode (group-by first behaviors)
        opcode-passed-fns (for [r (group-passed-fns-by-opcode cases-by-opcode)] r)
        opcodes (figure-opcodes (into [] opcode-passed-fns))
        opcode-map (into {} (for [op opcodes] op))
        test-codes (parse-test-program "day16-testprogram.txt")]
    (execute-test-program opcode-map test-codes)
    )
  )

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1))
      (println "Part 2")
      (time (part-2)))))