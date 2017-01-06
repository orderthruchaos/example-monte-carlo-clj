(ns stats.core)


(defn avg
  "Create a sample average given either a collection of numbers or a function,
  count, and (optional) arguments."
  ([c]          (let [n (count c)]
                  (/ (reduce + c) n)))
  ([f n & args] (let [c (take n (apply f args))]
                  (/ (reduce + c) n)))
  )

(defn std-dev
  "Create a sample standard deviation given either a collection of numbers or
  a function, count, and (optional) arguments."
  ([c] (let [n (count c)
             a (avg c)
             ;; quick squaring function
             q #(* % %)
             ;; Sum of squares of the difference from the average
             s (reduce + (map #(q (- % a)) c))
             ;; Or, could do:
             s2 (->>
                  c
                  (map #(- % a))
                  (map #(* % %))
                  (reduce +))
             ]
         (Math/sqrt (/ s (dec n)))))
  ([f n & args] (let [c (take n (apply f args))]
                  (std-dev c)))
  )

(defn avg-and-std-dev
  "Return the average and standard deviation given either a collection of
  numbers or a function, count, and (optional) arguments."
  ([f n & args] (let [c (take n (apply f args))]
                  (avg-and-std-dev c)))
  ([c] (let [a (avg c)
             s (std-dev c)]
         {:average a, :std-dev s}))
  )
