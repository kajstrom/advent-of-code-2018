(ns clj.day21
  (:require [clj.day19 :refer [name-to-fn parse-instructions]]))

(defn collect-terminators [cnt register ip instructions]
  (loop [register register
         counter 0
         terminators [0]
         loop-cnt 0]
    (let [instr-idx (get register ip)
          instr (get instructions instr-idx)
          ]
      (when (= 28 instr-idx) (println "Diff to previous" (- (nth register 4) (last terminators))))
      (if (nil? instr)
        terminators
        (if-not (= (count terminators) cnt)
          (let [
                fn-name(first instr)
                fn (get name-to-fn fn-name)
                register-after (apply fn register (rest instr))
                terminators (if (= 28 instr-idx) (conj terminators (nth register 4)) terminators)
                ]
            (recur (update register-after ip inc) (inc counter) terminators (inc loop-cnt))
            )
          terminators)))))

(defn part-1 []
  (let [[[ip] instructions] (parse-instructions "day21.txt")]
    (second (collect-terminators 2 [0 0 0 0 0 0] ip instructions))))