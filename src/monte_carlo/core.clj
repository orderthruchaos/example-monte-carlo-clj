(ns monte-carlo.core
  (:require
    [monte-carlo.simulation :refer :all]
    [bnd.utils :refer :all]
    [clojure.pprint :refer :all]
    )
  )


(defn- process-step-fn [step-fn r sim-state-prev]
  (let [user-state-prev (get-state sim-state-prev)]
    (->> (step-fn user-state-prev r)
         (set-state sim-state-prev))
    )
  )


(defn- process-step-fn-indexed [step-fn r sim-state-prev]
  (let [user-state-prev (get-state sim-state-prev)
        i               (get-step  sim-state-prev)]
    (->> (step-fn user-state-prev r i)
         (set-state sim-state-prev)
         inc-step)
    )
  )


; (defn- process-path! [rand-fn! step-fn n-steps user-state-0 j]
;   (let [sim-state-0 (initial-state user-state-0 j)]
;     (reduce
;       (fn [a _] (process-step-fn step-fn (first (rand-fn!)) a))
;       sim-state-0
;       (range (inc n-steps)))
;     )
;   )

(defn- process-path! [rand-fn! step-fn n-steps user-state-0 j]
  (reduce
    (fn [s _]
      (step-fn s (first (rand-fn!))))
    user-state-0
    (range (inc n-steps))))


(defn- process-path-indexed! [rand-fn! step-fn n-steps user-state-0 j]
  (let [sim-state-0 (initial-state user-state-0 j)]
    (reduce
      (fn [a _] (process-step-fn-indexed step-fn (first (rand-fn!)) a))
      sim-state-0
      (range (inc n-steps)))
    )
  )

;; step-fn must be of the form:
;;   (fn [current-state random-number current-step-index] ,,,
;;     new-state)
(defn simulate-indexed! [rand-fn! step-fn n-steps m-paths user-state-0]
  "Perform monte-carlo simulation.

  step-fn must be of the form:
    (fn [current-state random-number current-step-index] ,,, new-state)
  where current-state and new-state are user-provided state values."
  (map #(get-state (process-path-indexed!
                     rand-fn! step-fn n-steps user-state-0 %))
       (range m-paths))
  )

;; step-fn must be of the form:
;;   (fn [current-state random-number current-step-index] ,,,
;;     new-state)
(defn p-simulate-indexed! [rand-fn! step-fn n-steps m-paths user-state-0]
  "Perform monte-carlo simulation.

  step-fn must be of the form:
    (fn [current-state random-number current-step-index] ,,, new-state)
  where current-state and new-state are user-provided state values."
  (pmap #(get-state (process-path-indexed!
                      rand-fn! step-fn n-steps user-state-0 %))
        (range m-paths))
  )

;; step-fn must be of the form:
;;   (fn [current-state random-number current-step-index] ,,,
;;     new-state)
(defn simulate! [rand-fn! step-fn n-steps m-paths user-state-0]
  "Perform monte-carlo simulation.

  step-fn must be of the form:
    (fn [current-state random-number current-step-index] ,,, new-state)
  where current-state and new-state are user-provided state values."
  (map #(process-path! rand-fn! step-fn n-steps user-state-0 %)
       (range m-paths))
  )

;; step-fn must be of the form:
;;   (fn [current-state random-number current-step-index] ,,,
;;     new-state)
(defn p-simulate! [rand-fn! step-fn n-steps m-paths user-state-0]
  "Perform monte-carlo simulation.

  step-fn must be of the form:
    (fn [current-state random-number current-step-index] ,,, new-state)
  where current-state and new-state are user-provided state values."
  (pmap #(process-path! rand-fn! step-fn n-steps user-state-0 %)
        (range m-paths))
  )
