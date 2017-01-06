(ns monte-carlo.stock-time-series
  (:require
    ; incanter.distributions
    [bnd.utils :refer :all]
    [monte-carlo.core :refer :all]
    [random.utils :refer :all]
    [clojure.pprint :refer :all]
    [monte-carlo.stock-price :refer :all]
    )
  )

(defn- ts-stepper
  "Add next price to the vector."
  [px-stepper ;; price stepper function
   s-prev     ;; vector of previous stock prices
   z          ;; standard normal sample
   ]
  (conj s-prev (px-stepper (last s-prev) z))
  )

(defn- simulate-time-series!
  [sig r div t-n s-0 n-steps m-paths par-fl]
  (let [dt         (calc-dt t-n n-steps)
        sim!       (if par-fl p-simulate! simulate!)
        px-stepper (next-stock-price-generator sig r div t-n n-steps)
        stepper    (partial ts-stepper px-stepper)
        ts         (time-steps t-n n-steps)
        vs         (sim! std-norm-rand! stepper n-steps m-paths [s-0])
        fvs        (flatten vs)
        ]
    {:max (apply max fvs)
     :min (apply min fvs)
     :val (map #(into [] (map (fn [t s] {:t t :s s}) ts %)) vs)}
    )
  )

(defn stock-time-series!
  "Create multiple paths of time series for stock prices."
  [sig     ;; volatility
   r       ;; risk-free rate
   div     ;; dividend
   t-n     ;; tenor
   s-0     ;; initial stock price
   n-steps ;; number of time steps
   m-paths ;; number of paths
   ]
  (simulate-time-series! sig r div t-n s-0 n-steps m-paths false)
  )

(defn p-stock-time-series!
  "Create multiple paths of time series for stock prices (parallelized)."
  [&
   [sig     ;; volatility
    r       ;; risk-free rate
    div     ;; dividend
    t-n     ;; tenor
    s-0     ;; initial stock price
    n-steps ;; number of time steps
    m-paths ;; number of paths
    :as args
    ]
   ]
  (let [vargs (conj (into [] args) true)]
    (apply simulate-time-series! vargs)
    )
  )

(defn try-ts! [] (stock-time-series! @example-vol 0.05 0.0 1.0 100.0 100 100))
(defn p-try-ts! [] (p-stock-time-series! @example-vol 0.05 0.0 1.0 100.0 100 100))

;; ;; Notes:
;; ((fn [& [a b :as vs]] (conj (apply vector vs) a)) 1 2 3 4)
;;=  [1 2 3 4 1]
;; ((fn [& [a b :as vs]] (conj (into [] vs) a)) 1 2 3 4)
;;=  [1 2 3 4 1]
