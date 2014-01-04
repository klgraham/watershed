(ns watershed.statistics
  (:require [watershed.distributions :as d]))

;;;;; Basic statistics functions

(defn mean
  "Computes the mean of a sequence of numbers."
  [coll]
  (let [n (count coll)]
    (/ (reduce + coll) n)))

(defn variance
  "Computes the variance of a sequence of numbers. There should not be issues
  with numerical stability here. See http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  for more info. This is the compensated-summation algorithm for the variance."
  [coll]
  (let [n (count coll)
        m (mean coll)
        sqr (fn [x] (* x x))
        x-m (map #(- % m) coll)
        x-m2 (map sqr x-m)
        sum1 (reduce + x-m)
        sum2 (reduce + x-m2)
        s (-> (sqr sum1) (/ n))]
    (-> (- sum2 s) (/ (dec n)))))

(defn stddev
  "Computes the standard deviation of a sequence of numbers."
  [coll]
  (Math/sqrt (variance coll)))

(defn summary
  "Computes summary statistics for a sequence of numbers."
  [coll]
  (let [m (mean coll)
        sigma (stddev coll)
        sigma2 (Math/sqrt sigma)]
    (println "Mean: " m)
    (println "Std. Dev.:" sigma)
    (println "Variance: " sigma2)))

;(println "\nGaussian")
;(def gaussian (d/dist :normal 10000))
;(println "Gaussian mean: " (mean gaussian))
;(println "Gaussian stddev: " (stddev gaussian))
;(println "Gaussian variance: " (variance gaussian))
;(summary gaussian)

;(println "\nChi-Squared")
;(def chi-sqr (d/dist :chi2 10000 1))
;(time (summary chi-sqr))
;(println (d/prob-dist chi-sqr (d/between? 1.95 2.05)))