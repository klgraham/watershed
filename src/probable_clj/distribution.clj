(ns probable-clj.distribution
  (:import (java.util Random)
           (java.lang Double))
  (:require [schema.core :as s]
            [clojure.core.reducers :as r])
  (:use [clojure.core.match :only (match)]))

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

(defn prob
  "Returns the probability that a random variable drawn from the
   distribution obeys the predicate. Defaults to 10,000 samples."
  [dist
   predicate?
   & {:keys [given? samples] :or {samples 10000}}]
  (let [d (if (nil? given?)
            (sample dist samples)
            (given dist given? samples))
        n (count d)]
    (-> (into [] (r/filter predicate? d))
        count
        (.doubleValue)
        (/ n))))

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

;;;; Implementations of specific distributions

;; Distribution with random variables uniformly distributed on [0,1].
(s/defrecord UniformDistribution
  [r :- Random]
  Distribution
  (sample [this] (.nextDouble r)))

(s/defn uniform :- UniformDistribution
  "Factory function to create a UniformDistribution"
  [] (UniformDistribution. (new Random)))

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

(s/defrecord ExponentialDistribution
  [r :- Random
   p :- Double]
  Distribution
  (sample [this] (* (/ -1 p) (Math/log (.nextDouble r)))))

(s/defn exponential :- ExponentialDistribution
  "Factory function to create a ExponentialDistribution"
  [p :- Double] (ExponentialDistribution. (new Random) p))

;; Distribution with random int uniformly distributed on [low, high].
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

;;; Probabilisic graphical model example.

;;; Here is a schematic for the model. The notation is as follows:
;;; {:x [] :y [:x]} means:
;;; 1) x has no dependencies => P(x)
;;; 2) y depends on x => P(y|x)
;;;
;;; If this is not clear, then try this:
;;;
;;;         X -> Y

(def pgm {:smart []
          :grades [:smart]
          :affluent []
          :high-sat [:smart :affluent :grades]
          :scholarship [:grades :high-sat]})

(def p-smart 0.4)
(def p-affluent 0.2)
(def p-grades-if-smart 0.7)
(def p-grades-if-not-smart 0.1)

(s/defn smart :- TrueFalseDistribution
  [] (true-false p-smart))

(s/defn affluent :- TrueFalseDistribution
  [] (true-false p-affluent))

(s/defn grades :- TrueFalseDistribution
  [is-smart :- Boolean]
  (if is-smart
    (true-false p-grades-if-smart)
    (true-false p-grades-if-not-smart)))

(s/defn high-sat :- TrueFalseDistribution
  [is-smart :- Boolean
   is-affluent :- Boolean
   good-grades :- Boolean]
  (match [is-smart is-affluent good-grades]
         [true true true] (true-false 0.8)
         [true false true] (true-false 0.7)
         [false true true] (true-false 0.2)
         [true _ false] (true-false 0.2)
         [false true false] (true-false 0.01)
         [false false true] (true-false 0.1)
         [false false false] (true-false 0.01)))

(s/defn scholarship :- TrueFalseDistribution
  [good-grades :- Boolean
   high-sat :- Boolean]
  (match [good-grades high-sat]
         [true true] (true-false 0.9)
         [true false] (true-false 0.4)
         [false true] (true-false 0.4)
         [false false] (true-false 0.0)))

(s/defrecord AcademicsDistribution []
  Distribution
  (sample [this]
          (let [s (.sample (smart))
                a (.sample (affluent))
                g (.sample (grades s))
                sat (.sample (high-sat s a g))
                sh (.sample (scholarship g sat))]
            {:smart s :affluent a :grades g :high-sat sat :scholarship sh})))

(s/defn academics [] (AcademicsDistribution.))

;;; Next, we'll use the same example as in
;;; http://jliszka.github.io/2013/12/18/bayesian-networks-and-causality.html

;; constants for priors
(def p-rush 0.2)
(def p-weather 0.05)
(def p-accident-bad-weather 0.3)
(def p-accident-good-weather 0.1)
(def p-sirens-accident 0.9)
(def p-sirens-no-accident 0.2)
(def p-rwa 0.95)

;; priors
(s/defn rush-hour :- TrueFalseDistribution
  [] (true-false p-rush))

(s/defn bad-weather :- TrueFalseDistribution
  [] (true-false p-weather))

;; conditionals
(s/defn accident :- TrueFalseDistribution
  [weather-is-bad :- Boolean]
  (if weather-is-bad
    (true-false p-accident-bad-weather)
    (true-false p-accident-good-weather)))

(s/defn sirens :- TrueFalseDistribution
  [accident :- Boolean]
  (if accident
    (true-false p-sirens-accident)
    (true-false p-sirens-no-accident)))

(s/defn traffic-jam :- TrueFalseDistribution
  [rush-hour bad-weather accident]
  (match [rush-hour bad-weather accident]
         [true true _] (true-false p-rwa)
         [true _ true] (true-false p-rwa)
         [_ true true] (true-false p-rwa)
         [true false false] (true-false 0.5)
         [false true false] (true-false 0.3)
         [false false true] (true-false 0.6)
         [false false false]  (true-false 0.1)))

(s/defrecord TrafficDistribution []
  Distribution
  (sample [this]
          (let [r (.sample (rush-hour))
                w (.sample (bad-weather))
                a (.sample (accident w))
                s (.sample (sirens a))
                t (.sample (traffic-jam r w a))]
            {:rush-hour r :bad-weather w :accident a
             :sirens s :traffic-jam t})))

(s/defn traffic [] (TrafficDistribution.))

(defn traffic-dist [n]
  (let [dist (atom [])]
    (doseq [r (sample (rush-hour) n)
            w (sample (bad-weather) n)
            a (sample (accident w) n)
            s (sample (sirens a) n)
            t (sample (traffic-jam r w a) n)]
      (swap! dist conj {:rush-hour r :bad-weather w :accident a
                        :sirens s :traffic-jam t}))
    (deref dist)))
