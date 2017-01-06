(ns bnd.utils)

;; Based on the definition of defn-
(defmacro def-
  "same as def, yielding non-public def"
  [name & decls]
  (let [m (assoc (meta name) :private true)]
    `(do 
       (def ~name ~@decls)
       (alter-meta! (var ~name) assoc :private true))
    ))

(def- default-tolerance (Math/pow 10.0, -10.0))

(defn within?
  "Returns true if expected and actual are equal within a given tolerance."
  ([expected actual]           (within? default-tolerance expected actual))
  ([tolerance expected actual]
   (> tolerance (Math/abs (double (- expected actual))))))

(defmacro is-within
  "Clean syntax for using 'within?' in tests."
  [& args]
  (if (not ((loaded-libs) 'clojure.test))
    (throw (ClassNotFoundException.
             (str 'is-within " requires clojure.test to be loaded.")))
    `(clojure.test/is (within? ~@args))))
(defmacro is-not-within [& args]
  "Clean syntax for using 'comp not within?' in tests."
  (if (not ((loaded-libs) 'clojure.test))
    (throw (ClassNotFoundException.
             (str 'is-not-within " requires clojure.test to be loaded.")))
    `(clojure.test/is (clojure.core/not (within? ~@args)))))

(def example-vol (atom 0.5))
(defn set-example-vol [v] (reset! example-vol v))
