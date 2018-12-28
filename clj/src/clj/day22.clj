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

(def type-allowed-eq
  {
   :rocky [:climbing :torch]
   :wet [:climbing :neither]
   :narrow [:torch :neither]
   })

(defn can-move-to? [region can-change-to-eqs]
  (let [region-allowed-eq ((:type region) type-allowed-eq)]
    (some #(in? region-allowed-eq %) can-change-to-eqs)))

(defn move-cost [equipment region]
  (let [region-allowed-eq ((:type region) type-allowed-eq)]
    (if (in? region-allowed-eq equipment)
      1
      7)))

(defn create-duration-starting-state [starting-node nodes]
  (let [default [Integer/MAX_VALUE nil]]
    (-> (into {} (for [node nodes] [(:id node) default]))
        (assoc (:id starting-node) [1 nil])
        transient)))

(defn possible-moves-from [[x y] regions]
  (let [can-change-to ((:type (get-in regions [y x])) type-allowed-eq)
        up (get-in regions [(dec y) x])
        down (get-in regions [(inc y) x])
        left (get-in regions [y (dec x)])
        right (get-in regions [y (inc x)])
        moves (filter #(not= nil %) [up down left right])]
    (filter #(can-move-to? % can-change-to) moves)))

(defn update-durations [durations current-node]
  (let [current-identifier (:id current-node)
        current-cost (-> (get durations current-identifier) first)]
    (doseq [move (:moves current-node)]
      (if (nil? move)
        durations
        (let [[node cost] move
              point (:id node)
              [point-current-cost] (get durations point)
              cost (+ current-cost cost)]
          (if (> point-current-cost cost)
            (assoc! durations point [cost current-identifier])))))))

(defn cheapest-non-visited [moves visited]
  (let [not-visited (filter #(not-in? visited (first %)) moves)
        min-cost (apply min Integer/MAX_VALUE (map last not-visited))]
    (first (filter #(= min-cost (last %)) not-visited))))

(defn create-region-nodes [region]
  (let [x (:x region)
        y (:y region)
        type (:type region)
        type-eq (type type-allowed-eq)]
    (for [eq type-eq]
      (transient {:coords [x y] :type type :equipment eq :moves [] :visited false
                  :id [x y eq]}))))

(defn find-nodes-for-coords [coords nodes]
  (filter #(= coords (:coords %)) nodes))

(defn region-to-nodes [regions specials]
  (let [nodes (map create-region-nodes (flatten regions))
        nodes (filter #(not (and (in? specials (:coords %)) (not= :torch (:equipment %)))) (flatten nodes))]
    (map (fn [node]
           (let [moves (possible-moves-from (:coords node) regions)]
             (assoc! node :moves (flatten-1 (for [move moves]
                                                   (let [move-nodes (find-nodes-for-coords [(:x move) (:y move)] nodes)]
                                                     (for [move-node move-nodes]
                                                       (if (= (:equipment node) (:equipment move-node))
                                                         [move-node 1]
                                                         [move-node 7]))))))
             )) nodes)))

(defn select-equipment [current-equipment current-region next-region]
  (println current-region next-region)
  (if (in? ((:type next-region) type-allowed-eq) current-equipment)
    current-equipment
    (let [current-allowed ((:type current-region) type-allowed-eq)
          next-allowed ((:type next-region) type-allowed-eq)]
      (first (filter #(in? current-allowed %) next-allowed)))))

(defn sort-queue [from queue]
  ;(println (count queue))
  (sort-by (juxt (comp (partial manhattan-distance (:coords from)) :coords first) last) queue)
  )

(defn find-fastest-path-to [starting-node all-nodes]
  (let [durations-to-points (create-duration-starting-state starting-node all-nodes)]
    (loop [current starting-node
           queue []
           iter 0
           timer (System/currentTimeMillis)]
      (when (= 0 (mod iter 100))                              ;(println (:coords current))
        (println iter "Remaining" (count queue) "Batch time " (- (System/currentTimeMillis) timer) "msecs")
        )
      (if (nil? current)
        durations-to-points
        (let [moves (filter #(not (:visited (first %))) (:moves current))
              queue (filter #(= false (:visited (first %))) queue)
              queue (concat queue moves)
              sorted-queue (sort-queue current queue)
              timer (if (= 0 (mod iter 100)) (System/currentTimeMillis) timer)]
          (update-durations durations-to-points current)
          (assoc! current :visited true)
          (recur (-> (first sorted-queue) first) (rest sorted-queue) (inc iter) timer))))))

(defn part-2 []
  (let [t-x 10
        t-y 10
        regions (create-rectangle-until-target 510 [(+ 10 t-x) (+ 0 t-y)])
        p-regions (vec (map persistent! (persistent! regions)))
        regions-as-nodes (doall (region-to-nodes p-regions [[0 0] [t-x t-y]]))]
    (get (find-fastest-path-to (first regions-as-nodes) regions-as-nodes) [t-x t-y :torch]))
  )
