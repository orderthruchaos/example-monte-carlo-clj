(ns monte-carlo.stock-price
  (:require
    [bnd.utils :refer :all]
    [monte-carlo.core :refer :all]
    [random.utils :refer :all]
    [clojure.pprint :refer :all]
    )
  )

;; See "Implementing Derivatives Models" by Les Clewlow and Chris Strickland.
;; Page 85.

;; T / n
(defn calc-dt
  "Calculate time step size."
  [t-n n] (/ t-n n))

;; (r - div - 0.5 * sig * sig) * dt
(defn- calc-nu-dt
  ([r div sig t-n n] (calc-nu-dt r div sig (calc-dt t-n n)))
  ([r div sig dt]    (* dt (- (- r div) (* 0.5 sig sig)))))

;; sig * sqrt(dt)
(defn- calc-sig-sqrt-dt
  ([sig t-n n] (calc-sig-sqrt-dt sig (calc-dt t-n n)))
  ([sig dt]    (* sig (Math/sqrt dt)))
  )

(defn- pre-proc-next-stock-price
  [dt     ;; tenor / number of time steps
   nu-dt  ;; from calc-nu-dt
   ssdt   ;; from calc-sig-sqrt-dt
   s-prev ;; log of previous stock price
   z      ;; standard normal sample
   ]
  (* s-prev (Math/exp (+ nu-dt (* ssdt z)))))

(defn- pre-proc-next-ln-stock-price
  [dt     ;; tenor / number of time steps
   nu-dt  ;; from calc-nu-dt
   ssdt   ;; from calc-sig-sqrt-dt
   ln-s-prev ;; log of previous stock price
   z      ;; standard normal sample
   ]
  (+ ln-s-prev nu-dt (* ssdt z)))

(defn next-ln-stock-price
  "Calculate next step in log-of-stock-price process."
  [sig    ;; volatility
   r      ;; risk-free rate
   div    ;; dividend
   t-n    ;; tenor
   n      ;; number of time steps
   ln-s-prev ;; log of previous stock price
   z      ;; standard normal sample
   ]
  (let [dt    (calc-dt t-n n)
        nu-dt (calc-nu-dt r div sig dt)
        ssdt  (calc-sig-sqrt-dt sig dt)]
    (pre-proc-next-ln-stock-price dt nu-dt ssdt ln-s-prev z)))

(defn next-stock-price
  "Calculate next step in stock price process."
  [sig    ;; volatility
   r      ;; risk-free rate
   div    ;; dividend
   t-n    ;; tenor
   n      ;; number of time steps
   s-prev ;; previous stock price
   z      ;; standard normal sample
   ]
  (let [dt    (calc-dt t-n n)
        nu-dt (calc-nu-dt r div sig dt)
        ssdt  (calc-sig-sqrt-dt sig dt)]
    (pre-proc-next-stock-price dt nu-dt ssdt s-prev z)))

(defn next-ln-stock-price-generator
  "Create a ln-stock price evolution function that only takes ln(S_prev) and
  a random distribution sample."
  [sig    ;; volatility
   r      ;; risk-free rate
   div    ;; dividend
   t-n    ;; tenor
   n      ;; number of time steps
   ]
  (let [dt    (calc-dt t-n n)
        nu-dt (calc-nu-dt r div sig dt)
        ssdt  (calc-sig-sqrt-dt sig dt)]
    (partial pre-proc-next-ln-stock-price dt nu-dt ssdt)))

(defn next-stock-price-generator
  "Create a stock price evolution function that only takes S_prev and
  a random distribution sample."
  [sig    ;; volatility
   r      ;; risk-free rate
   div    ;; dividend
   t-n    ;; tenor
   n      ;; number of time steps
   ]
  (let [dt    (calc-dt t-n n)
        nu-dt (calc-nu-dt r div sig dt)
        ssdt  (calc-sig-sqrt-dt sig dt)]
    (partial pre-proc-next-stock-price dt nu-dt ssdt)))

(defn model-stock-price!
  "Create a stock price evolution function that only takes S_prev and
  a random distribution sample."
  [sig     ;; volatility
   r       ;; risk-free rate
   div     ;; dividend
   t-n     ;; tenor
   s-0     ;; initial stock price
   n-steps ;; number of time steps
   m-paths ;; number of paths
   ]
  (let [stepper (next-stock-price-generator sig r div t-n n-steps)]
    (simulate! std-norm-rand! stepper n-steps m-paths s-0)))

(defn p-model-stock-price!
  "Create a stock price evolution function that only takes S_prev and
  a random distribution sample."
  [sig     ;; volatility
   r       ;; risk-free rate
   div     ;; dividend
   t-n     ;; tenor
   s-0     ;; initial stock price
   n-steps ;; number of time steps
   m-paths ;; number of paths
   ]
  (let [stepper (next-stock-price-generator sig r div t-n n-steps)]
    (p-simulate! std-norm-rand! stepper n-steps m-paths s-0)))

(defn time-steps
  "Create list of time steps."
  [t-final n]
  (let [dt (calc-dt t-final n)]
    (map #(* % dt) (range (inc n)))))


(defn try-model!   []
  (model-stock-price! @example-vol 0.05 0.0 1.0 100.0 100 100))
(defn p-try-model! []
  (p-model-stock-price! @example-vol 0.05 0.0 1.0 100.0 100 100))

#_(do
    (load-file "src/monte_carlo/stock_price.clj") (use 'monte-carlo.stock-price)
    (try-model!)
    )
