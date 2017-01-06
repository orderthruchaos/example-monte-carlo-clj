(ns monte-carlo.simulation-test
  (:require [clojure.test :refer :all]
            [monte-carlo.simulation :refer :all]
            [bnd.utils :refer :all])
  (:import  (monte_carlo.simulation SimulationState))
  ; (:import  ('monte-carlo.simulation SimulationState))
  )


(def- ss-a-0-0 (->SimulationState :a 0 0))
(def- ss-a-0-1 (->SimulationState :a 0 1))
(def- ss-b-1-2 (->SimulationState :b 1 2))


(deftest constructor-test
  (testing "Basic constructor."
    (is (= ss-b-1-2 (map->SimulationState {:state :b
                                           :step  1
                                           :path  2}))))
  (testing "State initializer."
    (is (= ss-a-0-0 (initial-state :a)))
    (is (= ss-a-0-1 (initial-state :a 1)))
    )
  )

(deftest getter-test
  (testing "Get state."
    (is (= :a (get-state ss-a-0-1)))
    (is (= :b (get-state ss-b-1-2))))
  (testing "Get step."
    (is (= 0 (get-step ss-a-0-1)))
    (is (= 1 (get-step ss-b-1-2))))
  (testing "Get path."
    (is (= 1 (get-path ss-a-0-1)))
    (is (= 2 (get-path ss-b-1-2))))
  )

(deftest setter-test
  (let [ss-c-0-1 (->SimulationState :c 0 1)
        ss-d-1-2 (->SimulationState :d 1 2)
        ss-a-0-5 (->SimulationState :a 0 5)
        ss-b-1-7 (->SimulationState :b 1 7)]
    (testing "Set state."
      (is (= ss-c-0-1 (set-state ss-a-0-1 :c)))
      (is (= ss-d-1-2 (set-state ss-b-1-2 :d))))
    (testing "Set path."
      (is (= ss-a-0-5 (set-path ss-a-0-1 5)))
      (is (= ss-b-1-7 (set-path ss-b-1-2 7))))
    )
  )

(deftest inc-step-test
  (testing "Increment step."
    (is (= 1 (get-step (inc-step ss-a-0-1))))
    (is (= 2 (get-step (inc-step ss-b-1-2))))
    (is (= 5 (-> ss-b-1-2 inc-step inc-step inc-step inc-step get-step)))
    )
  )
