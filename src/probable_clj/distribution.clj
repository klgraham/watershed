(ns probable-clj.distribution
  (:import (java.util Random)
           (java.lang Double))
  (:require [schema.core :as s]))

(defprotocol Distribution
  "Basic interface for a probability distribution"
  (sample [this] [this n]
          "Draws one or n values from the distribution.
          Returns either a single value or a vector of values."))

;; Distribution with random variables uniformly distributed on [0,1].
(s/defrecord UniformDistribution
  [r :- Random]
  Distribution
  (sample [this] (.nextDouble r))
  (sample [this n] (into [] (repeatedly n #(.sample this)))))

(defn uniform
  "Factory function to create a UniformDistribution"
  [] (UniformDistribution. (new Random)))

;(def u (Uniform. (new Random)))
;(def u (uniform))
;(def uu (.sample u 5))
;(println uu)
;(def uu1 (map (fn [x] (inc x)) uu))
;(println uu1)

;; Distribution with random variables uniformly distributed on [0,1].
(s/defrecord TrueFalseDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (< (.nextDouble r) p))
  (sample [this n] (into [] (repeatedly n #(.sample this)))))

(s/defn true-false
  "Factory function to create a TrueFalseDistribution"
  [p :- Double] (TrueFalseDistribution. (new Random) p))

;(def tf (true-false 0.2))
;(println (.sample tf 5))

(s/defrecord BernoulliDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (let [b (< (.nextDouble r) p)]
                   (if b 1 0)))
  (sample [this n] (into [] (repeatedly n #(.sample this)))))

(s/defn bernoulli
  "Factory function to create a BernoulliDistribution"
  [p :- Double] (BernoulliDistribution. (new Random) p))

;(def bern (bernoulli 0.5))
;(println (.sample bern 5))