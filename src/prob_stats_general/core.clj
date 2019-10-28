(ns prob-stats-general.core
  (:gen-class)
  (:use [clojure.math.numeric-tower :only (sqrt)]))


(defn avg
  "Returns average of the items in a collection.
   Computes average based on vals of maps if coll is a map."
  [coll]
  (if (empty? coll)
    0
    (let [new-coll (if (map? coll)
                     (vals coll) coll)
          size (count new-coll)]
      (/ (reduce + new-coll) size))))

(defn variance [coll]
  (let [sum (reduce + coll)
        sum-of-squares (reduce + (map #(* % %) coll))
        sum-squared (* sum sum)
        size (count coll)]
    (/ (- sum-of-squares (/ sum-squared size)) (dec size))))

(defn std-dev [var] (math/sqrt var))

(defn factorial
  ([n sum]
   (if (= n 1)
     sum
     (recur (dec n) (*' sum (dec n)))))
  ([n] (if (<= n 1) 1 (factorial (dec n) (*' n (dec n))))))

(defn n-choose-k
  "n!/(k!(n-k)!)"
  [n k]
  (double (/ (factorial n) (factorial k) (factorial (- n k)))))


(defn b-pmf
  "Binomial-Product Mass Function:
  n!/(k!(n-k)!)*((p)^x)((1-p)^(n-x))"
  [x n p]
  (if (> x n)
    0
    (* (n-choose-k n x)
       (apply * (repeat x p)) ; p^x
       (apply * (repeat (- n x) (- 1 p)))))) ; (1-p)^(n-x)

(defn nb-pmf
  "Negative Binomial-Product Mass Function:
  (x+r-1)!/((r-1)!x!)*((p)^r)((1-p)^x)"
  [x r p]
  (* (n-choose-k (+ x r -1) (- r 1))
     (apply * (repeat r p)) ; p^r
     (apply * (repeat x (- 1 p))))) ; (1-p)^x

(defn h-pmf
  "Hypergeometric Binomial-Product Mass Function:
   ((M!/(x!(M-x)!)*(N-M)!)*((N-M)!/(N-M-(n-x))!))/(N!/(N!(N-n)!))"
  [x n M N]
  (/ (* (n-choose-k M x) (n-choose-k (- N M) (- n x))) (n-choose-k N n)))

(defn h-mean
  [n M N]
  (double (* n (/ M N))))

(defn nb-mean
  [r p]
  (/ (* r (- 1 p)) p))

(defn -main
  [& args]
  (do (println "Type fn to list functions or type name of function"
               "and respective parameters to compute results.")
      (let [in (read-line)]
        (if (re-find #"factorial \d+ \d+" in )
          (prn (factorial (re-find #"\d+ \d+" in)))
          (prn "not found")))))
