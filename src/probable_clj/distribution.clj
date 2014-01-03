(ns probable-clj.distribution
  (:import (java.util Random)
           (java.lang Double Boolean)
           (sun.security.x509 DistributionPoint))
  (:require [schema.core :as s]
            [clojure.core.reducers :as r])
  (:use [clojure.core.match :only (match)]
        [taoensso.timbre :only (info)]))

; for logging
;(timbre/refer-timbre)

;;;; Probability distribution and the functions that operate on them
;;;; Two types of distributions are represented here:
;;;; (1) Continuous => distribution is a vector of values
;;;; (2) Discrete => distribution is a hash-map of values to their frequencies
;;;;                 Exceptions to this are trivial distributions, like the
;;;;                 DiscreteDistribution or the Coin below. This is really for
;;;;                 distributions where possible values are composites, like in
;;;;                 graphical models.

; todo: combine the predicate? in given into sample: (sample u 10 :given pred?)
(defprotocol Distribution
  "Basic specification for a probability distribution"
  (sample [this] "Draws a single value from the distribution."))

(defprotocol Coin
  "Basic functions for simulating coin tosses."
  (flip [this]
        "Flips a coin once, returning 'H or 'T"))

(defn flatten2
  "Like `clojure.core/flatten` but better, stronger, faster.
  Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat, lazy sequence.
  If the argument is non-sequential (numbers, maps, strings, nil,
  etc.), returns the original argument."
  {:static true}
  [x]
  (letfn [(flat [coll]
                (lazy-seq
                  (when-let [c (seq coll)]
                    (let [x (first c)]
                      (if (sequential? x)
                        (concat (flat x) (flat (rest c)))
                        (cons x (flat (rest c))))))))]
    (if (sequential? x) (flat x) x)))

(defn sum-samples
  "Given samples drawn from a discrete distribution it sums the frequencies and
  returns the output. Given samples from a continuous distribution the output
  is returned unchanged."
  [dist]
  (let [first-sample (first dist)]
    (if (map? first-sample)
      (into {} (reduce #(merge-with + %1 %2) dist))
      dist)))

(s/defn metropolis-sampling :- clojure.lang.PersistentHashMap
  "Samples the given distribution using Monte Carlo with Metropolis-Hastings
  sampling"
  [dist :- Distribution
   num-samples :- s/Int]
  (let [current-state (atom (.sample dist))
        samples (atom {@current-state 1})
        ;current-prob (atom 1)
        acceptance-ratio (atom 0)]
    (doseq [n (range 1 num-samples)]
      (let [proposed-state (.sample dist)
            proposal-prob (get @samples proposed-state 1)
            current-prob (double (get @samples @current-state))
            r (rand)
            ratio (/ current-prob proposal-prob)
            accept? (< r ratio)
            state-to-update (if accept? proposed-state @current-state)]
        (swap! samples update-in [state-to-update] (fnil inc 0))
        (reset! current-state state-to-update)
        (if accept?
          (swap! acceptance-ratio inc))))
    (info "Acceptance ratio: " (/ (double @acceptance-ratio) num-samples))
    (info "Number of samples: " num-samples)
    @samples))

(s/defn sample :- clojure.lang.PersistentVector
  "Draws n values from the distribution."
  [dist :- Distribution
   n :- s/Int]
  (match (class dist)
         GrassDistribution  (metropolis-sampling dist n)
         :else (sum-samples (repeatedly n #(.sample dist)))))

(defn given
  "Returns a vector of n values that fulfill the predicate."
  [dist
   predicate?
   ;n
   & {:keys [pgm?] :or {pgm? false}}]
  ; Because of the way the distribution for PGMs are being generated, the
  ; (.sample) fn draws more than one possible state from the PGM distribution
  (let [num-samples (if pgm? 5000 10000)
        fresh-coll (if pgm? {} [])]
    (->> (sample dist num-samples)
         (r/filter predicate?)
         (into fresh-coll))))

(s/defn flip :- clojure.lang.PersistentVector
  "Draws n values from the distribution."
  [coin :- Coin
   n :- s/Int]
  (into [] (repeatedly n #(.flip coin))))

(defn prob
  "Returns the probability that a random variable drawn from the
   distribution obeys the predicate."
  [dist
   predicate?
   & {:keys [given? debug] :or {given? #(-> (nil? %) not) debug false}}]
  (let [test-sample (.sample dist)
        pgm? (not (or (number? test-sample) (symbol? test-sample)))
        num-samples (if pgm? 5000 10000)
        all (sample dist num-samples)
        d (given dist (every-pred given? predicate?) :pgm? pgm?)
        numerator (-> (if pgm? (reduce + (vals d)) (count d))
                      (.doubleValue))
        denom (if pgm?
                (r/fold + (r/map val (into [] (r/filter given? all))))
                (count (into [] (r/filter given? all))))]
    (if debug (println "\tPGM?: " pgm? ", numerator: " numerator ", denom: " denom) nil)
    (/ numerator denom)))

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

(s/defn tf?
  "Returns a function that will operate on a hash-map/tuple where the key is itself
  a hash-map and return true if the key-of-interest has value true. Used for
  PGM-type distributions."
  [key-of-interest :- s/Keyword
   tf :- Boolean]
  (fn [map] (= tf (get (get map 0) key-of-interest false))))


;;;; Implementations of specific distributions

;; Distribution with random variables uniformly distributed on [0,1).
(s/defrecord UniformDistribution
  [r :- Random]
  Distribution
  (sample [this] (.nextDouble r)))

(s/defn uniform :- UniformDistribution
  "Factory function to create a UniformDistribution"
  [] (UniformDistribution. (new Random)))

;; Distribution with random variables normally distributed.
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

;;;
;;; Probabilisic graphical model example.

;;; Here is a schematic for the model. The notation is as follows:
;;; {:x [] :y [:x]} means:
;;; 1) x has no dependencies => P(x)
;;; 2) y depends on x => P(y|x)
;;;
;;; If this is not clear, then try this:
;;;
;;;         X -> Y

;(def pgm {:smart []
;          :grades [:smart]
;          :affluent []
;          :high-sat [:smart :affluent :grades]
;          :scholarship [:grades :high-sat]})

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

(defn traffic-map
  "Possible state of the traffic Bayesian network. For each distinct set of
  values [r w a s t], we have a distinct state of the Bayesian network."
  [r w a s t]
  {:rush-hour r :bad-weather w :accident a :sirens s :traffic-jam t})

; A convenient way to specify the probability distribution for a Bayesian
; network like this is to sample the distribution and specify frequencies
; for each state. This way it will become easy to compute probabilities
; without memory issues.
;(s/defn initialize-traffic-dist
;  "There are 2^5 = 32 possible states for this Bayesian Net. Each will
;  be initialized with frequency zero."
;  []
;  (let [bayes-net (atom {})]
;    (doseq [r [true false]
;            w [true false]
;            a [true false]
;            s [true false]
;            t [true false]]
;      (swap! bayes-net assoc-in [(traffic-map r w a s t)] 0))
;    (deref bayes-net)))

(s/defn traffic-dist :- clojure.lang.PersistentHashMap
  [rush-hour :- Boolean
   bad-weather :- Boolean
   n :- s/Int]
  (frequencies
    (for [a (sample (accident bad-weather) n)
          s (sample (sirens a) n)
          t (sample (traffic-jam rush-hour bad-weather a) n)]
      (traffic-map rush-hour bad-weather a s t))))

; todo: Don't think this is the best way to gnerate this distribution, regarding how to iterate over the states.
(s/defrecord TrafficDistribution
  [rush-hour :- TrueFalseDistribution
   bad-weather :- TrueFalseDistribution]
  Distribution
  (sample [this]
          (let [r (.sample rush-hour)
                w (.sample bad-weather)]
            (traffic-dist r w 5))))

(s/defn traffic [] (TrafficDistribution. (rush-hour) (bad-weather)))

;; Another example from http://www.cs.ubc.ca/~murphyk/Bayes/bnintro.html
(s/defn cloudy :- TrueFalseDistribution
  [] (true-false 0.5))

(s/defn sprinkler :- TrueFalseDistribution
  [cloudy :- Boolean]
  (if cloudy
    (true-false 0.1)
    (true-false 0.5)))

(s/defn rain :- TrueFalseDistribution
  [cloudy :- Boolean]
  (if cloudy
    (true-false 0.8)
    (true-false 0.2)))

(s/defn wet-grass :- TrueFalseDistribution
  [sprinkler :- Boolean
   rain :- Boolean]
  (match [sprinkler rain]
         [true true] (true-false 0.99)
         [true false] (true-false 0.9)
         [false true] (true-false 0.9)
         [false false] (true-false 0.0)))

(defn grass-map
  "Possible state of the Grass Bayesian network. For each distinct set of
  values [c s r w], we have a distinct state of the Bayesian network."
  [c s r w]
  {:cloudy c :sprinkler s :rain r :wet-grass w})

;(s/defn grass-dist
;  [cloudy :- Boolean
;   n :- s/Int]
;  (frequencies
;    (for [s (sample (sprinkler cloudy) n)
;          r (sample (rain cloudy) n)
;          w (sample (wet-grass s r) n)]
;      (grass-map cloudy s r w))))

(s/defrecord GrassDistribution
  []
  Distribution
  (sample [this]
          (let [c (.sample (cloudy))
                s (.sample (sprinkler c))
                r (.sample (rain c))
                w (.sample (wet-grass s r))]
            (grass-map c s r w))))

(s/defn grass [] (GrassDistribution.))

