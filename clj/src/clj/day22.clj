(ns clj.day22
  (:require [clj.shared :refer :all]))

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

(defn create-rectangle-row [y depth regions target bounds]
  (let [row (transient [])
        regions (conj! regions row)
        x-indexes (range 0 (inc (first bounds)))]
    (loop [x-indexes x-indexes]
      (if (empty? x-indexes)
        regions
        (do
          (conj! row (create-region (first x-indexes) y depth regions target))
          (recur (rest x-indexes))
          )))
    ))

(defn create-rectangle-until-target [depth target bounds]
  (let [regions (transient [])]
    (loop [y 0]
      (if (> y (last bounds))
        regions
        (do
          (create-rectangle-row y depth regions target bounds)
          (recur (inc y)))))))

(def type-to-risk {
                   :rocky 0
                   :wet 1
                   :narrow 2
                   })

(defn calculate-region-risk-level [region]
  (apply + (map #(% type-to-risk) (map :type (flatten region)))))

(defn part-1 []
  (let [regions (create-rectangle-until-target 11541 [14 778] [14 778])
        p-regions (map persistent! (persistent! regions))]
    (calculate-region-risk-level p-regions)))

(def type-allowed-eq
  {
   :rocky [:climbing :torch]
   :wet [:climbing :neither]
   :narrow [:torch :neither]
   })

(defn can-move-to? [region can-change-to-eqs]
  (let [region-allowed-eq ((:type region) type-allowed-eq)]
    (some #(in? region-allowed-eq %) can-change-to-eqs)))

(defn possible-moves-from [[x y] regions]
  (let [can-change-to ((:type (get-in regions [y x])) type-allowed-eq)
        up (get-in regions [(dec y) x])
        down (get-in regions [(inc y) x])
        left (get-in regions [y (dec x)])
        right (get-in regions [y (inc x)])
        moves (filter #(not= nil %) [up down left right])]
    (filter #(can-move-to? % can-change-to) moves)))

(defn update-durations [current-node]
  (let [current-identifier (:id current-node)
        current-cost (:dist current-node)]
    (doseq [move (:moves current-node)]
      (let [[node cost] move
            point-current-cost (:dist node)
            new-cost (+ current-cost cost)]
        (if (> point-current-cost new-cost)
          (assoc! node :dist new-cost :prev current-identifier))
        ))))

(defn create-region-nodes [region]
  (let [x (:x region)
        y (:y region)
        type (:type region)
        type-eq (type type-allowed-eq)]
    (for [eq type-eq]
      (transient {:coords [x y] :type type :equipment eq :moves [] :dist Integer/MAX_VALUE, :prev nil
                  :id (str x "," y eq)}))))

(defn find-nodes-for-coords [coords nodes]
  (filter #(= coords (:coords %)) nodes))

(defn region-to-nodes [regions]
  (let [nodes (map create-region-nodes (flatten regions))
        nodes (flatten nodes)
        ]
    (map (fn [node]
           (let [moves (possible-moves-from (:coords node) regions)]
             (assoc! node :moves (flatten-1 (for [move moves]
                                                   (let [move-nodes (find-nodes-for-coords [(:x move) (:y move)] nodes)]
                                                     (for [move-node move-nodes]
                                                       (if (= (:equipment node) (:equipment move-node))
                                                         [move-node 1]
                                                         [move-node 8]))))))
             )) nodes)))

(defn find-fastest-path-to [starting-node all-nodes]
  (println "Start" (:id starting-node))
  (assoc! starting-node :dist 0)
  (let []
    (loop [queue (set all-nodes)
           iter 0
           timer (System/currentTimeMillis)]
      (when (= 0 (mod iter 100))
        (println iter "Remaining" (count queue) "Batch time " (- (System/currentTimeMillis) timer) "msecs")
        )
      (if (empty? queue)
        nil
        (let [current (reduce (fn [a b] (if (< (:dist a) (:dist b)) a b)) queue)
              timer (if (= 0 (mod iter 100)) (System/currentTimeMillis) timer)]
          (update-durations current)
          (recur (disj queue current) (inc iter) timer))))))

; Almost working :(
(defn part-2 []
  (let [t-x 14
        t-y 778
        regions (create-rectangle-until-target 11541 [t-x t-y] [(+ 15 t-x) (+ 0 t-y)])
        p-regions (vec (map persistent! (persistent! regions)))
        regions-as-nodes (doall (region-to-nodes p-regions))
        target1 (str t-x "," t-y :torch)
        target2 (str t-x "," t-y :climbing)]
    (find-fastest-path-to (second regions-as-nodes) regions-as-nodes)
    [(:dist (first (filter #(= target1 (:id %)) regions-as-nodes)))
     (+ 7 (:dist (first (filter #(= target2 (:id %)) regions-as-nodes))))
     ]
    )
  )
