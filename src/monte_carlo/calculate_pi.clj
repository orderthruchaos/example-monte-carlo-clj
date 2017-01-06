(ns monte-carlo.calculate-pi
  (:require
    [monte-carlo.core :refer :all]
    [bnd.utils :refer :all]
    [random.utils :refer :all]
    [clojure.pprint :refer :all]
    [incanter.core :as ic]
    [incanter.charts :as ch]
    )
  )


(defn- r-squared [[x y]] (+ (* x x) (* y y)))
(defn- proc-path [s-prev pair]
  ; (pprint s-prev)
  ; (pprint pair)
  (if (< (r-squared pair) 1)
    (inc s-prev)
    s-prev))

(defn- proc-path-with-pairs [s-prev pair]
  (let [ps (conj (s-prev :pairs) pair)
        r2 (r-squared pair)
        pc (s-prev :count)
        nc (if (< r2 1) (inc pc) pc)]
    {:pairs ps, :count nc}))

(defn approximate-pi!
  "Approximate pi."
  ([] (approximate-pi! 1000 100))
  ([n-steps m-paths]
   (let [s-finals (simulate!
                    uni-pairs-rand! proc-path n-steps m-paths 0)
         s-total  (reduce + 0.0 s-finals)
         ratio    (/ s-total (* n-steps m-paths))]
     (* 4 ratio))))

(defn approximate-pi-2!
  "Approximate pi (parallelized)."
  ([]  (approximate-pi-2! 1000 1))
  ([n] (approximate-pi-2! n 1))
  ([n-steps m-paths]
   (let [s-init   {:pairs [], :count 0}
         s-finals (simulate!
                    uni-pairs-rand!
                    proc-path-with-pairs n-steps m-paths s-init)
         summary  (reduce #(hash-map
                             :pairs (concat (:pairs %1) (:pairs %2))
                             :count (+ (:count %1) (:count %2))) s-finals)
         n-tot    (* n-steps m-paths)
         pi-apx   (* 4.0 (/ (:count summary) n-tot))
         ]
     (assoc summary :n n-tot :pi pi-apx))))

(defn p-approximate-pi-2!
  "Approximate pi and retain points."
  ([]  (approximate-pi-2! 1000 1))
  ([n] (approximate-pi-2! n 1))
  ([n-steps m-paths]
   (let [s-init   {:pairs [], :count 0}
         s-finals (p-simulate!
                    uni-pairs-rand!
                    proc-path-with-pairs n-steps m-paths s-init)
         summary  (reduce #(hash-map
                             :pairs (concat (:pairs %1) (:pairs %2))
                             :count (+ (:count %1) (:count %2))) s-finals)
         n-tot    (* n-steps m-paths)
         pi-apx   (* 4.0 (/ (:count summary) n-tot))
         ]
     (assoc summary :n n-tot :pi pi-apx))))


(defn p-approximate-pi!
  "Approximate pi and retain points (parallelized)."
  ([] (approximate-pi! 1000 100))
  ([n-steps m-paths]
   (let [s-finals (p-simulate!
                    uni-pairs-rand! proc-path n-steps m-paths 0)
         s-total  (reduce + 0.0 s-finals)
         ratio    (/ s-total (* n-steps m-paths))]
     (* 4 ratio))))

(defn pairs-to-points [pairs]
  "Convert pairs to points for Incanter dataset."
  (map #(hash-map :x (first %) :y (last %)) pairs))

(defn pi-sample-set!
  "Create a quick sample set of pairs."
  [n]
  (map #(hash-map :x (first %) :y (last %)) (take n (uni-pairs-rand!))))

(defn try-reg! [] (time (approximate-pi!   10000 1000)))
(defn try-par! [] (time (p-approximate-pi! 10000 1000)))

(defn chart-pi-sample-set!
  "Plot points from a simulation approximating pi."
  ([]     (chart-pi-sample-set! 1000 nil))
  ([n]    (chart-pi-sample-set! n    nil))
  ([n fp] (let [res    (approximate-pi-2! n)
                points (pairs-to-points (:pairs res))
                pi-apx (:pi    res)
                title  (str "Approximate pi (" n " points; " pi-apx ")")
                d-set  (ic/to-dataset points)
                s-plot (ch/scatter-plot
                         (ic/sel d-set :cols :x)
                         (ic/sel d-set :cols :y)
                         :title title
                         :x-label "x"
                         :y-label "y")
                s-plot (ch/set-x-range s-plot 0 1)
                s-plot (ch/set-y-range s-plot 0 1)
                f-plot (ch/add-function
                         s-plot
                         #(Math/sqrt (- 1 (* % %)))
                         0 1)
                ]
            (when (not (nil? fp)) (ic/save f-plot fp :width 700 :height 700))
            (ic/view f-plot :width 700 :height 700)
            )
   )
  )

(defn save-chart-pi!
  "Quick-n-dirty way to create and save a pi-chart."
  [n]
  (let [fp (str "pi-" n ".png")]
    (println fp)
    (chart-pi-sample-set! n fp)))

; user=> (try-reg!)
; "Elapsed time: 7955.653529 msecs"
; 3.1418568
; user=> (try-par!)
; "Elapsed time: 1882.098026 msecs"
; 3.142266

#_(do
    (load-file "src/monte_carlo/calculate_pi.clj")
    (use 'monte-carlo.calculate-pi)
    (chart-pi-sample-set!)
    )
