(ns probable-clj.statistics
  (:require [probable-clj.distributions :as d]))

;;;;; Basic statistics functions

(defn mean
  "Computes the mean of a sequence of numbers."
  [coll]
  (let [n (count coll)]
    (-> (reduce + coll) (/ n))))

(defn variance
  "Computes the variance of a sequence of numbers."
  [coll]
  (let [n (count coll)
        sum (reduce + coll)
        sqr-sum (->> (map #(* % %) coll)
                     (reduce +))
        mean-sum-sqr (/ n (* sum sum))]
    (/ (dec n) (- sqr-sum mean-sum-sqr))))

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
;(def gaussian (d/dist :gaussian 10000))
;(println "Gaussian mean: " (mean gaussian))
;(println "Gaussian stddev: " (stddev gaussian))
;(println "Gaussian variance: " (variance gaussian))
;(summary gaussian)

;(println "\nChi-Squared")
;(def chi-sqr (d/chi-squared 6 500000))
;(time (summary chi-sqr))
;;(println (d/prob-dist chi-sqr (d/between? 1.95 2.05)))