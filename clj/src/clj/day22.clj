(ns clj.day22
  (:require [clj.shared]))

(defn make-region [x y geo-index erosion-level]
  (let [type (case (mod erosion-level 3)
               0 :rocky
               1 :wet
               2 :narrow)]
    {:x x :y y :geo-index geo-index :erosion-level erosion-level :type type}))

(defn get-in! [m ks]
  (let [row (get m (first ks))
        rem (rest ks)]
    (if (= 1 (count rem))
      (get row (last rem))
      (get-in! row rem))))

(defn geo-index-of [x y regions target]
  (if (or (= [0 0] [x y]) (= target [x y]))
    0
    (if (= y 0)
      (* x 16807)
      (if (= x 0)
        (* y 48271)
        (let [ero1 (:erosion-level (get-in! regions [y (dec x)]))
              ero2 (:erosion-level (get-in! regions [(dec y) x]))]
          (* ero1 ero2))))))

(defn create-region [x y depth regions target]
  (let [geo-index (geo-index-of x y regions target)
        erosion-level (mod (+ geo-index depth) 20183)]
    (make-region x y geo-index erosion-level)))

(defn create-rectangle-row [y depth regions target]
  (let [row (transient [])
        regions (conj! regions row)
        x-indexes (range 0 (inc (first target)))]
    (loop [x-indexes x-indexes]
      (if (empty? x-indexes)
        regions
        (do
          (conj! row (create-region (first x-indexes) y depth regions target))
          (recur (rest x-indexes))
          )))
    ))

(defn create-rectangle-until-target [depth target]
  (let [regions (transient [])]
    (loop [y 0]
      (if (> y (last target))
        regions
        (do
          (create-rectangle-row y depth regions target)
          (recur (inc y)))))))

(def type-to-risk {
                   :rocky 0
                   :wet 1
                   :narrow 2
                   })

(defn calculate-region-risk-level [region]
  (apply + (map #(% type-to-risk) (map :type (flatten region)))))

(defn part-1 []
  (let [regions (create-rectangle-until-target 11541 [14 778])
        p-regions (map persistent! (persistent! regions))]
    (calculate-region-risk-level p-regions)))