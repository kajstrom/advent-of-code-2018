(ns clj.day19
  (:require [clj.shared :refer :all]
            [clojure.string :as s]
            [clj.day16 :refer [addr addi
                               mulr muli
                               banr bani
                               borr bori
                               setr seti
                               gtir gtri gtrr
                               eqir eqri eqrr]]))

(def name-to-fn {
                  "addr" addr "addi" addi
                  "mulr" mulr "muli" muli
                  "banr" banr "bani" bani
                  "borr" borr "bori" bori
                  "setr" setr "seti" seti
                  "gtir" gtir "gtri" gtri "gtrr" gtrr
                  "eqir" eqir "eqri" eqri "eqrr" eqrr
                  })

(defn parse-instruction [instr-str]
  (let [pieces (s/split instr-str #" ")]
    (apply conj [(first pieces)] (map parse-int (rest pieces)))))

(defn parse-instructions [file]
  (let [lines (split-lines-from-file file)
        ip (Character/digit (get (first lines) 4) 10)]
    [[ip] (vec (map parse-instruction (rest lines)))]))

(defn execute-until-halt [register ip instructions]
  (loop [register register
         counter 0]
    (let [instr-idx (get register ip)
          instr (get instructions instr-idx)]
      (if (nil? instr)
        register
        (let [
              fn-name(first instr)
              fn (get name-to-fn fn-name)
              register-after (apply fn register (rest instr))]
          (recur (update register-after ip inc) (inc counter))
          )))))

(defn execute-times [times register ip instructions]
  (loop [register register
         counter 0]
    (let [instr-idx (get register ip)
          instr (get instructions instr-idx)]
      (if (= counter times)
        register
        (let [
              fn-name(first instr)
              fn (get name-to-fn fn-name)
              register-after (apply fn register (rest instr))]
          (recur (update register-after ip inc) (inc counter))
          )))))

(defn part-1 []
  (let [[[ip] instructions] (parse-instructions "day19.txt")
        register [0 0 0 0 0 0]]
    (execute-until-halt register ip instructions)))

(defn part-2 []
  (let [[[ip] instructions] (parse-instructions "day19.txt")
        register [1 0 0 0 0 0]
        register-after (execute-times 100 register ip instructions)
        register3 (nth register-after 3)]
    (apply + (filter #(= (mod register3 %) 0) (range 1 (inc register3))))))

(defn time-results []
  (time
    (do
      (println "Part 1")
      (time (part-1))
      (println "Part 2")
      (time (part-2)))))