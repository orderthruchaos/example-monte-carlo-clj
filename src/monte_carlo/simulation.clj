(ns monte-carlo.simulation
  (:require
    [bnd.utils :refer :all]
    )
  )

(defrecord SimulationState [state step path])
(defn initial-state
  "Create a new SimulationState record.
  s-0 is the user-defined state.
  j is the path index."
  ([s-0]   (->SimulationState s-0 0 0))
  ([s-0 j] (->SimulationState s-0 0 j))
  )
(defn get-state
  "Get user-defined state information."
  [^SimulationState s]
  (:state s))
(defn set-state
  "Set user-defined state information."
  [^SimulationState s n-s]
  (assoc s :state n-s))
(defn get-path
  "Get the path index."
  [^SimulationState s]
  (:path s))
(defn set-path
  "Set the path index."
  [^SimulationState s j]
  (assoc s :path j))
(defn get-step
  "Get the step index."
  [^SimulationState s]
  (:step s))
(defn inc-step
  "Increment the step index."
  [^SimulationState s]
  (assoc s :step (inc (:step s))))
