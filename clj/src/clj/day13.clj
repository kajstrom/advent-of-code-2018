(ns clj.day13
  (:require [clj.shared :refer :all]))

(defn track-from-file [file]
  (split-lines-from-file file))

(defn parse-track-row [row]
  (vec (map (fn [path]
              (case path
                \- :straight
                \| :straight
                \/ :curve-r
                \\ :curve-l
                \+ :intersection
                \^ :straight
                \> :straight
                \< :straight
                \v :straight
                nil)) (seq (char-array row)))))

(def turns [:left :straight :right])

(def card-id-counter (atom 0))

(defn make-cart [x y direction]
  (let [id @card-id-counter]
    (swap! card-id-counter inc)
    (transient {:x x :y y :direction direction :next-turn-idx 0 :id id})))

(defn parse-carts [rows]
  (->> (map-indexed (fn [y-idx row]
                      (map-indexed (fn [x-idx track]
                                     (case track
                                       \^ (make-cart x-idx y-idx :north)
                                       \v (make-cart x-idx y-idx :south)
                                       \> (make-cart x-idx y-idx :east)
                                       \< (make-cart x-idx y-idx :west)
                                       nil)) row))
                    (map #(seq (char-array %)) rows))
       flatten
       (filter (complement nil?))))

(def directions-and-turns-to-new-dir
  {:north {
           :left :west
           :right :east
           }
   :south {
           :left :east
           :right :west
           }
   :west {
          :left :south
          :right :north
          }
   :east {
          :left :north
          :right :south
          }})

(def curves
  {
   :curve-l {
                 :east :south
                 :south :east
                 :west :north
                 :north :west
                 }
   :curve-r {
                 :east :north
                 :west :south
                 :south :west
                  :north :east
                 }
   })

(defn turn-cart-intersection [cart]
  (let [direction (:direction cart)
        turn-direction (nth turns (:next-turn-idx cart))
        next-turn-idx (mod (inc (:next-turn-idx cart)) 3)
        new-direction (if-not (= :straight turn-direction) (get-in directions-and-turns-to-new-dir [direction turn-direction]) direction)]
    ;(println (:id cart) direction turn-direction next-turn-idx new-direction)
    (assoc! cart :direction new-direction :next-turn-idx next-turn-idx)))

(defn turn-cart-curve [curve cart]
  (let [direction (:direction cart)]
    (assoc! cart :direction (get-in curves [curve direction]))))

(defn move-cart [cart]
  (let [direction (:direction cart)
        x (:x cart)
        y (:y cart)]
    (case direction
      :north (assoc! cart :y (dec y))
      :south (assoc! cart :y (inc y))
      :west (assoc! cart :x (dec x))
      :east (assoc! cart :x (inc x))
      (println "Id" (:id cart) "Dir" (:direction cart) "X" (:x cart) "Y" (:y cart)))))

(defn find-first-crash [all-carts]
  (loop [current-cart (first all-carts)
         carts-to-test (rest all-carts)]
    (if-not (nil? current-cart)
             (let [all-except-current (filter #(not= (:id current-cart) (:id %)) all-carts)
                   crashes (filter #(and (= (:x current-cart) (:x %)) (= (:y current-cart) (:y %))) all-except-current)]
               (if (empty? crashes)
                 (recur (first carts-to-test) (rest carts-to-test))
                 [(:x current-cart) (:y current-cart)]))
             nil)))

(defn move-carts-until-crash [track carts]
  (loop [cart (first carts)
         remaining-carts (rest carts)]
    (if-not (nil? cart)
      (do
        (move-cart cart)
        (let [track-part (get-in track [(:y cart) (:x cart)])]
          (if (= :intersection track-part) (turn-cart-intersection cart))
          (if (or (= :curve-l track-part) (= :curve-r track-part)) (turn-cart-curve track-part cart))
          (let [crash (find-first-crash carts)]
            (if (nil? crash)
              (recur (first remaining-carts) (rest remaining-carts))
              crash))
          )))))

(defn move-carts-ticks-until-crash [track carts]
  (loop [crash nil
         ticks 0]
    (println ticks)
    (if (nil? crash)
      (recur (move-carts-until-crash track carts) (inc ticks))
      crash)))

(defn part-1 []
  (let [track-rows (track-from-file "day13.txt")
        track (vec (map parse-track-row track-rows))
        carts (parse-carts track-rows)]
    (move-carts-ticks-until-crash track carts)))