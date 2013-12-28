(ns probable-clj.distribution
  (:import (java.util Random)
           (java.lang Double))
  (:require [schema.core :as s]))

;;;; Probability distribution and the functions that operate on them
; todo: combine the predicate? in given into sample: (sample u 10 :given pred?)
(defprotocol Distribution
  "Basic specification for a probability distribution"
  (sample [this] "Draws a single value from the distribution."))

(defprotocol Coin
  "Basic functions for simulating coin tosses."
  (flip [this]
        "Flips a coin once, returning 'H or 'T"))

(s/defn sample :- clojure.lang.PersistentVector
  "Draws n values from the distribution."
  [dist :- Distribution
   n :- s/Int]
  (into [] (repeatedly n #(.sample dist))))

(s/defn sample-given
  "Draw a value from the distribution that fulfills the predicate."
  [dist :- Distribution
   predicate?]
  (let [a (.sample dist)]
    (if (predicate? a) a (sample-given dist predicate?))))

(s/defn given :- clojure.lang.PersistentVector
  "Returns a vector of n values that fulfill the predicate."
  [dist :- Distribution
   predicate?
   n :- s/Int]
  (into [] (repeatedly n #(sample-given dist predicate?))))

(s/defn flip :- clojure.lang.PersistentVector
  "Draws n values from the distribution."
  [coin :- Coin
   n :- s/Int]
  (into [] (repeatedly n #(.flip coin))))

(s/defn prob :- Double
  "Returns the probability that a random variable drawn from the sample
   distribution dist-sample obeys the predicate. Defaults to 10,000 samples."
  [dist :- clojure.lang.PersistentVector
   predicate? & [number-of-samples]]
  (let [samples (if (nil? number-of-samples) 10000 number-of-samples)
        d (sample dist samples)
        n (count d)]
    (-> (filter predicate? d)
        count
        (/ (.doubleValue n)))))

;; Useful predicates
(s/defn gt? "Is x greater than y?"
  [y :- s/Number]
  (fn [x] (> x y)))

(s/defn gteq? "Is x greater than or equal to y?"
  [y :- s/Number]
  (fn [x] (>= x y)))

(s/defn lt? "Is x less than y?"
  [y :- s/Number]
  (fn [x] (< x y)))

(s/defn lteq? "Is x less than or equal to y?"
  [y :- s/Number]
  (fn [x] (<= x y)))

(s/defn between?
  "Is x in [low, high]"
  [low :- s/Number
   high :- s/Number]
  (fn [x] (and (>= x low)
               (<= x high))))

(s/defn eq? [y :- s/Number] (fn [x] (= x y)))

;; Distribution with random variables uniformly distributed on [0,1].
(s/defrecord UniformDistribution
  [r :- Random]
  Distribution
  (sample [this] (.nextDouble r)))

(s/defn uniform :- UniformDistribution
  "Factory function to create a UniformDistribution"
  [] (UniformDistribution. (new Random)))

;(def u (Uniform. (new Random)))
;(def u (uniform))
;(def uu (.sample u 5))
;(println uu)
;(def u-pos (-> u (.given #(< % 0.2) 5)))
;(def u-pos (.given u #(< % 0.2) 5))
;(def uu1 (map (fn [x] (inc x)) uu))
;(println u-pos)

;; Distribution with random variables normally distributed on (-\inf, \inf).
;; Has mean 0 and variance 1.
(s/defrecord StandardNormalDistribution
             [r :- Random]
  Distribution
  (sample [this] (.nextGaussian r)))

(s/defn normal :- StandardNormalDistribution
  "Factory function to create a StandardNormalDistribution"
  [] (StandardNormalDistribution. (new Random)))

;; Distribution with random true/false variables, with probability p of being true.
(s/defrecord TrueFalseDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (< (.nextDouble r) p)))

(s/defn true-false :- TrueFalseDistribution
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
  (sample [this] (if (< (.nextDouble r) p) 1 0)))

(s/defn bernoulli :- BernoulliDistribution
  "Factory function to create a BernoulliDistribution"
  [p :- Double] (BernoulliDistribution. (new Random) p))

;(def bern (bernoulli 0.5))
;(println (.sample bern 5))

(s/defrecord ExponentialDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (* (/ -1 p) (Math/log (.nextDouble r)))))

(s/defn exponential :- ExponentialDistribution
  "Factory function to create a ExponentialDistribution"
  [p :- Double] (ExponentialDistribution. (new Random) p))

;; Distribution with random inte uniformly distributed on [low, high].
(s/defrecord DiscreteUniformDistribution
  [low :- s/Int
   high :- s/Int]
  Distribution
  (sample [this] (rand-nth (range low (inc high)))))

(s/defn discrete-uniform :- DiscreteUniformDistribution
  "Factory function to create a DiscreteUniformDistribution"
  [low :- s/Int
   high :- s/Int] (DiscreteUniformDistribution. low high))

;"A pair of n-sided dice. Each die is identical and has sides
;consecutively numbered from low to high"
(s/defrecord PairOfNSidedDice
  [low :- s/Int
   high :- s/Int]
  Distribution
  ;"Given a roll [a b], returns a + b"
  (sample [this]
    (let [r (range low (inc high))
          one (rand-nth r)
          two (rand-nth r)]
      (+ one two))))

(s/defn dice :- PairOfNSidedDice
  "Factory function to create a DiscreteUniformDistribution"
  [low :- s/Int
   high :- s/Int] (PairOfNSidedDice. low high))

; A fair coin, produces values 'H or 'T
(s/defrecord CoinDistribution [r :- Random]
  Distribution
  (sample [this] (if (< (.nextDouble r) 0.5) 'H 'T))

  Coin
  (flip [this] (.sample this)))

(s/defn coin :- CoinDistribution
  [] (CoinDistribution. (new Random)))

;; A biased coin, with probability p of coming up heads.
(s/defrecord BiasedCoinDistribution [r :- Random p :- Double]
  Distribution
  (sample [this] (if (< (.nextDouble r) p) 'H 'T))

  Coin
  (flip [this] (.sample this)))

(s/defn biased-coin :- BiasedCoinDistribution
  "Factory function to create a BiasedCoinDistribution"
  [p :- Double] (BiasedCoinDistribution. (new Random) p))

