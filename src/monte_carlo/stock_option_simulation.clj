(ns monte-carlo.stock-option-simulation
  (:require
    ; incanter.distributions
    [bnd.utils :refer :all]
    [monte-carlo.core :refer :all]
    [random.utils :refer :all]
    [clojure.pprint :refer :all]
    [monte-carlo.stock-price :refer :all]
    [monte-carlo.stock-option :refer :all]
    )
  )

(defn simulate-euro-call!
  "Simulate a European call."
  [sigma   ;; volatility
   r       ;; risk-free rate
   div     ;; dividend
   t-n     ;; tenor
   s-0     ;; initial stock price
   k       ;; strike price
   n-steps ;; number of time steps
   m-paths ;; number of paths
   ]
  (let [prices (model-stock-price! sigma r div t-n s-0 n-steps m-paths)
        bsm-px (bsm-call s-0 k r div sigma t-n)
        sim-px (euro-call-price r t-n 0 k prices)
        bsm-st (str "BSM: " bsm-px)
        sim-st (str "Sim: " sim-px)
        ]
    (pprint bsm-st)
    (pprint sim-st)
    ))

(defn p-simulate-euro-call!
  "Simulate a European call (parallelized)."
  [sigma   ;; volatility
   r       ;; risk-free rate
   div     ;; dividend
   t-n     ;; tenor
   s-0     ;; initial stock price
   k       ;; strike price
   n-steps ;; number of time steps
   m-paths ;; number of paths
   ]
  (let [prices (p-model-stock-price! sigma r div t-n s-0 n-steps m-paths)
        bsm-px (bsm-call s-0 k r div sigma t-n)
        sim-px (euro-call-price r t-n 0 k prices)
        bsm-st (str "BSM: " bsm-px)
        sim-st (str "Sim: " sim-px)
        ]
    (pprint bsm-st)
    (pprint sim-st)
    ))

(defn simulate-euro-put!
  "Simulate a European put."
  [sigma   ;; volatility
   r       ;; risk-free rate
   div     ;; dividend
   t-n     ;; tenor
   s-0     ;; initial stock price
   k       ;; strike price
   n-steps ;; number of time steps
   m-paths ;; number of paths
   ]
  (let [prices (model-stock-price! sigma r div t-n s-0 n-steps m-paths)
        bsm-px (bsm-put s-0 k r div sigma t-n)
        sim-px (euro-put-price r t-n 0 k prices)
        ; bsm-st (str "BSM: " bsm-px)
        ; sim-st (str "Sim: " sim-px)
        ]
    ; (pprint bsm-st)
    ; (pprint sim-st)
    {:bsm bsm-px, :sim sim-px}
    ))

(defn p-simulate-euro-put!
  "Simulate a European put (parallelized)."
  [sigma   ;; volatility
   r       ;; risk-free rate
   div     ;; dividend
   t-n     ;; tenor
   s-0     ;; initial stock price
   k       ;; strike price
   n-steps ;; number of time steps
   m-paths ;; number of paths
   ]
  (let [prices (p-model-stock-price! sigma r div t-n s-0 n-steps m-paths)
        bsm-px (bsm-put s-0 k r div sigma t-n)
        sim-px (euro-put-price r t-n 0 k prices)]
    {:bsm bsm-px, :sim sim-px}
    ))

; Quick and dirty simulators.
(defn quick-euro-call! []
  (simulate-euro-call! @example-vol 0.05 0 1 100 100 10000 10000))
(defn p-quick-euro-call! []
  (p-simulate-euro-call! @example-vol 0.05 0 1 100 100 10000 10000))
(defn quick-euro-put! []
  (simulate-euro-put! @example-vol 0.05 0 1 100 100 10000 10000))
(defn p-quick-euro-put! []
  (p-simulate-euro-put! @example-vol 0.05 0 1 100 100 10000 10000))

; Notes
#_(do
    (load-file "src/monte_carlo/stock_option_simulation.clj")
    (use 'monte-carlo.stock-option-simulation)
    (time (quick-euro-call!))
    (time (p-quick-euro-call!))
    (time (quick-euro-put!))
    (time (p-quick-euro-put!))

    (println "% In MATLAB:
             bnd_black_scholes
             example_vol=0.5
             bndBS_callPrice(100, 100, 0.05, example_vol, 1)
             bndBS_putPrice(100, 100, 0.05, example_vol, 1)")
    )
