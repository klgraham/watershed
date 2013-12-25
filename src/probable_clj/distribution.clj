(ns probable-clj.distribution
  (:import (java.util Random)
           (java.lang Double))
  (:require [schema.core :as s]))

;; Basic specification of a probability distribution
(defprotocol Distribution
  "Basic interface for a probability distribution"
  (sample [this] [this n]
          "Draws one or n values from the distribution.
          Returns either a single value or a vector of values.")
  (sample-given [this predicate?]
         "Returns a value that fulfills the predicate.")
  (given [this predicate? n]
         "Returns a vector of n values that fulfill the predicate."))

;; Distribution with random variables uniformly distributed on [0,1].
(s/defrecord UniformDistribution
  [r :- Random]
  Distribution
  (sample [this] (.nextDouble r))
  (sample [this n] (into [] (repeatedly n #(.sample this))))
  (sample-given [this predicate?]
                (let [a (.sample this)]
                  (if (predicate? a) a (sample-given this predicate?))))
  (given [this predicate? n]
         (into [] (repeatedly n #(.sample-given this predicate?)))))

(defn uniform
  "Factory function to create a UniformDistribution"
  [] (UniformDistribution. (new Random)))

;(def u (Uniform. (new Random)))
(def u (uniform))
;(def uu (.sample u 5))
;(println uu)
(def u-pos (-> u (.given #(< % 0.2) 5)))
;(def u-pos (.given u #(< % 0.2) 5))
;(def uu1 (map (fn [x] (inc x)) uu))
(println u-pos)

;; Distribution with random variables normally distributed on (-infinity, inifinity).
;; Has mean 0 and variance 1.
(s/defrecord StandardNormalDistribution
             [r :- Random]
  Distribution
  (sample [this] (.nextGaussian r))
  (sample [this n] (into [] (repeatedly n #(.sample this))))
  (sample-given [this predicate?]
                (let [a (.sample this)]
                  (if (predicate? a) a (sample-given this predicate?))))
  (given [this predicate? n]
         (into [] (repeatedly n #(.sample-given this predicate?)))))

(s/defn normal
  "Factory function to create a StandardNormalDistribution"
  [] (StandardNormalDistribution. (new Random)))

;; Distribution with random true/false variables, with probability p of being true.
(s/defrecord TrueFalseDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (< (.nextDouble r) p))
  (sample [this n] (into [] (repeatedly n #(.sample this))))
  (sample-given [this predicate?]
                (let [a (.sample this)]
                  (if (predicate? a) a (sample-given this predicate?))))
  (given [this predicate? n] (into [] (repeatedly n #(.sample-given this predicate?)))))

(s/defn true-false
  "Factory function to create a TrueFalseDistribution"
  [p :- Double] (TrueFalseDistribution. (new Random) p))

;(def tf (true-false 0.2))
;(println (.sample tf 5))

;; Distribution with random 1/0 variables, with probability p of being 1.
;; 1 == hit/win
;; 0 == miss/loss
;; p == probability of a hit/win
(s/defrecord BernoulliDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (let [b (< (.nextDouble r) p)]
                   (if b 1 0)))
  (sample [this n] (into [] (repeatedly n #(.sample this))))
  (sample-given [this predicate?]
                (let [a (.sample this)]
                  (if (predicate? a) a (sample-given this predicate?))))
  (given [this predicate? n] (into [] (repeatedly n #(.sample-given this predicate?)))))

(s/defn bernoulli
  "Factory function to create a BernoulliDistribution"
  [p :- Double] (BernoulliDistribution. (new Random) p))

;(def bern (bernoulli 0.5))
;(println (.sample bern 5))

(s/defrecord ExponentialDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (* (/ -1 p) (Math/log (.nextDouble r))))
  (sample [this n] (into [] (repeatedly n #(.sample this))))
  (sample-given [this predicate?]
                (let [a (.sample this)]
                  (if (predicate? a) a (sample-given this predicate?))))
  (given [this predicate? n] (into [] (repeatedly n #(.sample-given this predicate?)))))

(s/defn exponential
  "Factory function to create a ExponentialDistribution"
  [p :- Double] (ExponentialDistribution. (new Random) p))

