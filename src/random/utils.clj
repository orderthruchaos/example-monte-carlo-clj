(ns random.utils
  (:require [bnd.utils :refer :all])
  (:import [java.util.concurrent ThreadLocalRandom]
           [java.util Random]
           ))

;; See:
;;   https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform#Polar_form


;; Final take:  Use Java's utility class to do all of the ugly work.

;; For examples in the presentation which use the standard-normal
;; distribution, this is all we really need!
(defn std-norm-rand!
  "Generate standard normal random numbers with thread-local random number
  generator."
  [] (repeatedly #(.nextGaussian (ThreadLocalRandom/current))))

;; A more complete variant:
(defn norm-rand!
  "Generate normal random numbers with thread-local random number generator.
  Cannot be seeded. Specify the mean with a :mu argument and the standard
  deviation with a :sigma argument.

  For example:  (norm-rand! :mu 20 :sigma 5)

  Mean defaults to 0; standard deviation defaults to 1."
  [& opts]
     (let [{:keys [mu    ;; mean
                   sigma ;; standard deviation
                   ]
            :or   {mu 0,
                   sigma 1}} opts
           f1     identity
           f2     (partial map #(+ mu (* sigma %)))
           f      (if (and (= 0 mu) (= 1 sigma)) f1 f2)
           ]
       (->> (repeatedly #(.nextGaussian (ThreadLocalRandom/current))) f)))

;; For examples in the presentation which use the standard-normal
;; distribution, this is all we really need!
(defn simple-uni-pairs-rand!
  "Generate pairs of uniformly distributed random numbers."
  []
  (->> (repeatedly #(.nextDouble (ThreadLocalRandom/current))) (partition 2)))

;; A more complete variant:
(defn uni-pairs-rand!
  "Generate pairs of uniformly distributed random numbers."
  [& opts]
     (let [{:keys [scale shift]
            :or   {scale 1, shift 0}} opts
           f1     identity
           f2     (partial map #(+ shift (* scale %)))
           f      (if (and (= 0 shift) (= 1 scale)) f1 f2)
           ]
       (->> (repeatedly #(.nextDouble (ThreadLocalRandom/current)))
            f
            (partition 2)
            )))
