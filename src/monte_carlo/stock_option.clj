(ns monte-carlo.stock-option
  (:require
    ;; incanter.distributions
    [bnd.utils :refer :all]
    [monte-carlo.core :refer :all]
    [random.utils :refer :all]
    [clojure.pprint :refer :all]
    [monte-carlo.stock-price :refer :all]
    )
  (:use [incanter.distributions :only (normal-distribution cdf)])
  )

(def ^:private std-norm-dist (normal-distribution))
;; (def ^:private std-norm-dist (incanter.distributions/normal-distribution))
;; (def ^:private cdf incanter.distributions/cdf)

;; e^(-r(T - t))
(defn- discount
  [r t-final t]
  (Math/exp (* -1.0 r (- t-final t))))


(defn- discount-payoff [payoff-fn r t-final t k ss]
  (let [n (count ss)
        payoff-tot (reduce + (map #(payoff-fn % k) ss))
        payoff-avg (/ payoff-tot n)
        dsc        (discount r t-final t)]
    (* dsc payoff-avg)))

(defn euro-call-payoff [s-final k] (max (- s-final k) 0))
(defn euro-put-payoff  [s-final k] (max (- k s-final) 0))
(def  euro-call-price  (partial discount-payoff euro-call-payoff))
(def  euro-put-price   (partial discount-payoff euro-put-payoff))


(defn- ssdt [sigma t-final t]
  (* sigma (Math/sqrt (- t-final t))))

;; ln(S/K) + (r - d + 0.5 * sigma * sigma) * (T - t)
(defn- d-1-num
  [s-t k r div sigma t-final t]
  (+ (Math/log (/ s-t k))
     (* (- t-final t)
        (+ (- r div)
           (* 0.5 sigma sigma)))))

;; d-1-num / ssdt
(defn- calc-d-1
  [s-t k r div sigma t-final t]
  (/ (d-1-num s-t k r div sigma t-final t)
     (ssdt sigma t-final t)))

;; d-1 - ssdt
(defn- calc-d-2
  [s-t k r div sigma t-final t]
  (- (calc-d-1 s-t k r div sigma t-final t)
     (ssdt sigma t-final t)))

;; Black-Scholes-Merton:
;;   https://en.wikipedia.org/wiki/Black%E2%80%93Scholes_model#Black-Scholes_formula

;; N(...) is standard normal CDF.
;; S * e^(-d(T-t)) * N(d-1) - K * e^(-r(T-t)) * N(d-2)
(defn bsm-call
  ([s-t k r div sigma t-final] (bsm-call s-t k r div sigma t-final 0))
  ([s-t k r div sigma t-final t]
   (let [d1 (calc-d-1 s-t k r div sigma t-final t)
         d2 (calc-d-2 s-t k r div sigma t-final t)
         dd (discount div t-final t)
         dr (discount r   t-final t)]
     (- (* s-t dd (cdf std-norm-dist d1))
        (* k   dr (cdf std-norm-dist d2)))
     )
   )
  )

;; N(...) is standard normal CDF.
;; K * e^(-r(T-t)) * N(-d2) - S * e^(-d(T-t)) * N(-d1)
(defn bsm-put
  ([s-t k r div sigma t-final] (bsm-put  s-t k r div sigma t-final 0))
  ([s-t k r div sigma t-final t]
   (let [d1 (calc-d-1 s-t k r div sigma t-final t)
         d2 (calc-d-2 s-t k r div sigma t-final t)
         dd (discount div t-final t)
         dr (discount r   t-final t)]
     (- (* k   dr (cdf std-norm-dist (* -1 d2)))
        (* s-t dd (cdf std-norm-dist (* -1 d1))))
     )
   )
  )
