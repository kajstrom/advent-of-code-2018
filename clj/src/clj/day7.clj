(ns clj.day7
  (:require [clj.shared :refer [split-lines-from-file]]))

(defn parse-requirements [step-str]
  {:step (get step-str 36) :pre-req (get step-str 5)})

(defn load-requirements [file]
  (->> (split-lines-from-file file)
       (map parse-requirements)))

(defn steps-from-requirements [reqs]
  (distinct (reduce (fn [acc req]
                      (conj acc (:step req) (:pre-req req))
                      ) [] reqs)))

(defn find-next-nodes [node nodes]
  (let [next-nodes (filter #(> (.indexOf (:pre-req %) (:step node)) -1) nodes)]
    (assoc node :next (map #(find-next-nodes % nodes) next-nodes))))

(defn node-tree-from-steps [reqs roots]
  (let [nodes (map (fn [step]
                     {:step (first step) :next [] :pre-req (map :pre-req (last step))}
                     ) (group-by :step reqs))
        root-node {:step "" :next (map #(find-next-nodes % nodes) (map #(hash-map :step % :pre-req []) roots))}]
    root-node
    ))

(defn find-root [reqs steps]
  (filter #(= (.indexOf (map :step reqs) %) -1) steps))

(defn next-non-traversed [next-nodes traversed]
  (let [non-traversed (filter #(= (.indexOf traversed (:step %)) -1) next-nodes)
        pre-reqs-met (filter (fn [node] (every? #(> (.indexOf traversed %) -1) (:pre-req node))) non-traversed)
        pre-reqs-not-met (filter (fn [node] (some #(= (.indexOf traversed %) -1) (:pre-req node))) non-traversed)
        ]
    (if-not (empty? pre-reqs-met)
      (let [next-name (char (apply min (map int (map :step pre-reqs-met))))]
        (first (filter #(= next-name (:step %)) pre-reqs-met))))))

(defn tree-steps
  ([tree]
   (let [traversed (atom [])]
     ;(tree-steps tree traversed)
     (doseq [tree (:next tree)]
       (tree-steps tree traversed))
     traversed))
  ([tree traversed]
    (swap! traversed conj (:step tree))
    (while (not (nil? (next-non-traversed (:next tree) @traversed)))
      (let [next-node (next-non-traversed (:next tree) @traversed)]
        (do
          (tree-steps next-node traversed))))
    traversed))

(defn old-part-1 []
  (let [reqs (load-requirements "day7.txt")
        steps (steps-from-requirements reqs)
        roots (find-root reqs steps)
        tree (node-tree-from-steps reqs roots)
        ]
    (apply str @(tree-steps tree))
    ))

; Everything above this is obsolete made with wrong understanding of the problem
; Strangely the example problem was successfully solved with this.

; New (and improved \o/) version

(defn roots-to-steps [steps roots]
  (concat steps (map #(hash-map :step % :pre-req []) roots)))

(defn find-next-step [steps]
  (let [reqs-done (filter #(empty? (:pre-req %)) steps)
        min-step (reduce #(if (< (int (:step %1)) (int (:step %2)))
                            %1
                            %2)  reqs-done)]
    min-step))

(defn satisfy-pre-req [steps req]
  (map (fn [step]
         (assoc step :pre-req (filter #(not= req %) (:pre-req step)))
         ) steps))

(defn remove-completed [step steps]
  (filter #(not= (:step step) (:step %)) steps))

(defn correct-order [steps]
  (let []
    (loop [steps-left steps
           order []]
      (if-not (empty? steps-left)
        (let [next-step (find-next-step steps-left)]
          (recur (remove-completed next-step (satisfy-pre-req steps-left (:step next-step)))
                 (conj order (:step next-step))))
        order))))

(defn part-1 []
  (let [reqs (load-requirements "day7.txt")
        steps (map (fn [step]
               {:step (first step) :pre-req (map :pre-req (last step))}
               ) (group-by :step reqs))
        roots (find-root reqs (steps-from-requirements reqs))]
    (apply str (correct-order (roots-to-steps steps roots)))))

(def step-add-value (atom 60))
(defn step-duration [step]
  (+ @step-add-value (inc (- (int (:step step)) (int \A)))))

(defn find-free-workers [workers steps-in-progress]
  (filter #(= (.indexOf (map :worker steps-in-progress) %) -1) (range 0 (count workers))))

(defn find-completed-steps [steps-in-progress]
  (filter #(empty? (:seconds-of-work %)) steps-in-progress))

(defn remove-completed-steps [completed-steps steps]
  (let [completed-steps (map :step completed-steps)]
    (filter #(= (.indexOf completed-steps (:step %)) -1) steps)))

(defn remove-pre-reqs [completed-steps steps-left]
  (let [completed-steps (map :step completed-steps)]
    (map (fn [step]
           (assoc step :pre-req (filter #(= (.indexOf completed-steps %) -1) (:pre-req step)))
           ) steps-left)))

(defn find-available-steps [steps-left]
  (sort-by :step (filter #(empty? (:pre-req %)) steps-left)))

(defn assign-steps-to-workers [free-workers available-steps steps-in-progress]
  (let [free-cnt (count free-workers)
        workers (atom free-workers)
        steps-to-assign (take free-cnt available-steps)]
    (apply conj steps-in-progress (for [step steps-to-assign]
                                    (let [worker (first @workers)]
                                      (reset! workers (rest @workers))
                                      (assoc step
                                        :seconds-of-work (range (step-duration step))
                                        :worker worker))))))

(defn increment-worker-timelines [workers steps-in-progress]
  (map-indexed (fn [idx worker]
         (let [step (first (filter #(= idx (:worker %)) steps-in-progress))]
           (if-not (nil? step)
             (conj worker (first (:seconds-of-work step)))
             (conj worker nil)))
         ) workers))

(defn decrement-work-left [steps-in-progress]
  (map (fn [step]
         (assoc step :seconds-of-work (rest (:seconds-of-work step))))  steps-in-progress))

(defn remove-assigned-from-steps-left [steps-in-progress steps-left]
  (let [steps-in-progress (map :step steps-in-progress)]
    (filter #(= (.indexOf steps-in-progress (:step %)) -1) steps-left)))

; Steps to do:
; 1) Get any completed steps in progress
; 2) Remove completed from in progress
; 3) Remove completed from steps-left :pre-req
; 4) Find any possible new steps to work on
; 5) Find any available workers (steps in progress define if worker is available)
; 6) If workers available assign work to free workers
; 7) Remove assigned steps from steps-left
; 8) Increment worker timeline, if working with step value second else nil
; 9) Update steps-in-progress to decrease work left
; 10) Loop back to 1)

(defn complete-steps [workers steps]
  (loop [steps-left steps
         steps-in-progress []
         workers workers]
    (if-not (and (empty? steps-left) (empty? steps-in-progress))
      (let [completed-steps (find-completed-steps steps-in-progress)
            steps-left (remove-completed-steps completed-steps steps-left)
            steps-left (remove-pre-reqs completed-steps steps-left)
            steps-in-progress (remove-completed-steps completed-steps steps-in-progress)
            available-steps (find-available-steps steps-left)
            free-workers (find-free-workers workers steps-in-progress)
            steps-in-progress (assign-steps-to-workers free-workers available-steps steps-in-progress)
            workers (increment-worker-timelines workers steps-in-progress)
            steps-in-progress (decrement-work-left steps-in-progress)
            steps-left (remove-assigned-from-steps-left steps-in-progress steps-left)]
        (recur steps-left
               steps-in-progress
               workers))
      workers)))

(defn part-2 []
  (reset! step-add-value 60)
  (let [reqs (load-requirements "day7.txt")
        steps (map (fn [step]
                     {:step (first step) :pre-req (map :pre-req (last step))}
                     ) (group-by :step reqs))
        roots (find-root reqs (steps-from-requirements reqs))
        workers [[] [] [] [] []]]
    (dec (count (first (complete-steps workers (roots-to-steps steps roots)))))
    ))

(defn time-results []
  (time
    (do
      (println "Part 1:")
      (time (part-1))
      (println "Part 2:")
      (time (part-2)))))