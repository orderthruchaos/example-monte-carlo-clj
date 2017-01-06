(ns bnd.utils-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [bnd.utils :refer :all])
  )

;; Note: This is a way to access private vars.
(def in-tol  (/ @#'bnd.utils/default-tolerance 10))
(def out-tol (* @#'bnd.utils/default-tolerance 10))
(def- t1 :a)
(def- ^{:foo :bar} t2 1)

;; From http://clojuredocs.org/clojure.core/def :
;; give function another name
(def- t3 map)
;; give macro another name
(def- #^{:macro true} t4 #'loop)

(deftest priv-def-private-meta-test
  (testing ":private is true: t1"
    (is (= true (:private (meta #'t1)))))
  (testing ":private is true: t2"
    (is (= true (:private (meta #'t2)))))
  (testing ":private is true: t3"
    (is (= true (:private (meta #'t3)))))
  (testing ":private is true: t4"
    (is (= true (:private (meta #'t4)))))
  )

(deftest priv-def-other-meta-test
  (testing ":foo is :bar: t2"
    (is (= :bar (:foo (meta #'t2)))))
  (testing ":macro is true: t4"
    (is (= true (:macro (meta #'t4)))))
  )

(deftest within-test
  (testing "True when within tolerance"
    (is (within? 1.0 1.0))
    (is (within? 1.0 (+ 1.0 in-tol)))
    (is (within? 1.0 (- 1.0 in-tol)))
    (is (within? 1.5 2.0 1.0))
    )
  (testing "False when outside of tolerance"
    (is (not (within? 1.1 1.0)))
    (is (not (within? 1.0 (+ 1.0 out-tol))))
    (is (not (within? 1.0 (- 1.0 out-tol))))
    (is (not (within? 1.5 10.0 0.0)))
    )
  )

(deftest is-within-test
  (testing "True when is-within tolerance"
    (is-within 1.0 1.0)
    (is-within 1.0 (+ 1.0 in-tol))
    (is-within 1.0 (- 1.0 in-tol))
    (is-within 1.5 2.0 1.0)
    )
  )

(deftest is-not-within-test
  (testing "True when is-not-within tolerance"
    (is-not-within 1.1 1.0)
    (is-not-within 1.0 (+ 1.0 out-tol))
    (is-not-within 1.0 (- 1.0 out-tol))
    (is-not-within 1.5 10.0 0.0)
    )
  )
