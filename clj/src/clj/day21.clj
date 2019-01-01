(ns clj.day21
  (:require [clj.day19 :refer [name-to-fn parse-instructions]]
            [clj.shared :refer :all]))

(defn run-until-loop [register ip instructions]
  (loop [register register
         terminators []
         ]
    (let [instr-idx (get register ip)
          instr (get instructions instr-idx)
          ]
      ;(when (= 28 instr-idx) (println "Loop count" loop-cnt "Prev terminator" prev-terminator))
      (let [
            fn-name(first instr)
            fn (get name-to-fn fn-name)
            register-after (apply fn register (rest instr))
            ]
        (if (= 28 instr-idx)
          (let [terminator (nth register 4)]
            (if (in? terminators terminator)
              terminators
              (do
                (println "Terminator" terminator)
                (recur (update register-after ip inc) (conj terminators terminator))))
          )
          (recur (update register-after ip inc) terminators))))))

(defn part-1-and-2 []
  (let [[[ip] instructions] (parse-instructions "day21.txt")
        terminators (run-until-loop [0 0 0 0 0 0] ip instructions)]
    (println "Part 1")
    (println (first terminators))
    (println "Part 2")
    (println (last terminators))))