(ns watershed.distribution
  (:import (java.util Random)
           (java.lang Double Boolean))
  (:require [schema.core :as s]
            [incanter.distributions :as d])
  (:use [clojure.core.match :only (match)]
        [taoensso.timbre :only (info)]))

(def num-iterations 50000)

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

(defprotocol PgmDistribution
  "Basic specification for a joint probability distribution of a PGM"
  (sample [this] "Draws a one value from the PGM joint distribution.")
  (state-prob [this state-map] "Returns the probability of a graph with state given by state-map")
  (prob-of-states [this] "Returns a hash-map where the keys are states and values are state probabilities."))

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

;(defn sum-samples
;  "Given samples drawn from a discrete distribution it sums the frequencies and
;  returns the output. Given samples from a continuous distribution the output
;  is returned unchanged."
;  [dist]
;  (let [first-sample (first dist)]
;    (if (map? first-sample)
;      (into {} (reduce #(merge-with + %1 %2) dist))
;      dist)))

(s/defn pgm-sampling :- clojure.lang.PersistentHashMap
  "This doesn't really sample the distribution, it just calculates the
  probability of each state."
  [dist :- PgmDistribution
   n :- s/Int]
  (.prob-of-states dist)
  ;(->> (.prob-of-states dist)
  ;     (map #(vector (key %) (int (* (val %) n))))
  ;     (into {}))
  )

(s/defn gibbs-sampling :- clojure.lang.PersistentHashMap
  "Samples the distribution as a markov chain."
  [dist :- PgmDistribution
   num-samples :- s/Int]
  (let [current-state (atom (.sample dist))
        samples (atom {})]
    (doseq [n (range 0 num-samples)]
      ; pick one variable at random from the current state
      ; sample a value for that variable from its conditional distribution
      ; in the current state
      (doseq [k (keys @current-state)]
        (let [new-k-value (.sampler dist k @current-state)
              new-k-state (assoc @current-state k new-k-value)]
          (reset! current-state new-k-state)))
      (if (> n (/ num-samples 4)) (swap! samples update-in [@current-state] (fnil inc 0)) ""))
    @samples))

; todo: This still needs to be tweaked.
(s/defn metropolis-sampling :- clojure.lang.PersistentHashMap
  "Samples the given distribution using Monte Carlo with Metropolis-Hastings
  sampling"
  [dist :- PgmDistribution
   num-samples :- s/Int]
  (let [current-state (atom (.sample dist))
        samples (atom {@current-state 1})
        ;current-prob (atom 1)
        acceptance-ratio (atom 0)]
    (doseq [n (range 1 num-samples)]
      (let [k (rand-nth (keys @current-state))
            new-k-value (.sampler dist k @current-state)
            proposed-state (assoc @current-state k new-k-value)
            ;proposed-state (.sample dist)
            p-new (.state-prob dist proposed-state)
            p-current (.state-prob dist @current-state)
            ;proposal-prob (-> (get @samples proposed-state 1)
            ;                  (/ (double (inc num-samples)))
            ;                  (* p-new))
            ;current-prob (-> (double (get @samples @current-state))
            ;                 (/ num-samples)
            ;                 (* p-current))
            ratio (/ p-new p-current)
            ;ratio (/ proposal-prob current-prob)
            alpha (min 1 ratio)
            r (rand)
            accept? (> ratio r)
            state-to-update (if accept? proposed-state @current-state)]
        (swap! samples update-in [state-to-update] (fnil inc 0))
        (reset! current-state state-to-update)
        (if (and accept? (> n (/ num-samples 2)))
          (swap! acceptance-ratio inc))))
    (info "Acceptance ratio: " (/ (double @acceptance-ratio) (- num-samples (/ num-samples 2))))
    (info "Number of samples: " num-samples)
    @samples))

(s/defn sample :- clojure.lang.PersistentVector
  "Draws n values from the distribution."
  [dist
   n :- s/Int]
  (if (extends? PgmDistribution (type dist))
    (pgm-sampling dist n)
    ;(gibbs-sampling dist n)
    ;(metropolis-sampling dist n)
    (repeatedly n #(.sample dist))))

(defn given
  "Returns a vector of n values that fulfill the predicate."
  [dist
   predicate?
   ;n
   & {:keys [pgm?] :or {pgm? false}}]
  ; Because of the way the distribution for PGMs are being generated, the
  ; (.sample) fn draws more than one possible state from the PGM distribution
  (let [num-samples num-iterations]
    (->> (sample dist num-samples)
         (filter predicate?))))

(s/defn flip :- clojure.lang.PersistentVector
  "Draws n values from the distribution."
  [coin :- Coin
   n :- s/Int]
  (into [] (repeatedly n #(.flip coin))))

(defn prob
  "Returns the probability that a random variable drawn from the
   distribution obeys the query predicate, given another condition."
  [dist
   query?
   & {:keys [given? debug] :or {given? (fn [x] true) debug false}}]
  (let [pgm? (extends? PgmDistribution (type dist))
        num-samples num-iterations
        all (sample dist num-samples)
        N (double (if pgm?
                     (reduce + (vals all))
                     (count all)))
        numerator-dist (given dist (every-pred given? query?) :pgm? pgm?)
        numerator (-> (if pgm? (reduce + (vals numerator-dist)) (count numerator-dist))
                      (double))
        denom (if pgm?
                  (reduce + (vals (filter given? all)))
                  (count (filter given? all)))]
    (if debug (println "\n\n\tPGM?: " pgm? ", numerator: " numerator ", denom: " denom) nil)
    (/ numerator denom)))

;;;; Implementations of specific distributions

;; Distribution with random variables uniformly distributed on [0,1).
(defrecord UniformDistribution
  [r]
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

(s/defrecord PoissonDistribution
  [n :- s/Int]
  Distribution
  (sample [this] (.draw (d/poisson-distribution n))))

(s/defn poisson :- PoissonDistribution
  "Factory function to create a PoissonDistribution"
  [n :- s/Int]
  (:pre [(pos? n)])
  (PoissonDistribution. n))

(s/defrecord BinomialDistribution
  [n :- s/Int
   p :- Double]
  Distribution
  (sample [this] (.draw (d/binomial-distribution n p))))

(s/defn binomial :- BinomialDistribution
  "Factory function to create a BinomialDistribution"
  [n :- s/Int
   p :- Double]
  (:pre [(and (pos? n) (pos? p))])
  (BinomialDistribution. n p))

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

;(def p-smart 0.4)
;(def p-affluent 0.2)
;(def p-grades-if-smart 0.7)
;(def p-grades-if-not-smart 0.1)
;
;(s/defn smart :- TrueFalseDistribution
;  [] (true-false p-smart))
;
;(s/defn affluent :- TrueFalseDistribution
;  [] (true-false p-affluent))
;
;(s/defn grades :- TrueFalseDistribution
;  [is-smart :- Boolean]
;  (if is-smart
;    (true-false p-grades-if-smart)
;    (true-false p-grades-if-not-smart)))
;
;(s/defn high-sat :- TrueFalseDistribution
;  [is-smart :- Boolean
;   is-affluent :- Boolean
;   good-grades :- Boolean]
;  (match [is-smart is-affluent good-grades]
;         [true true true] (true-false 0.8)
;         [true false true] (true-false 0.7)
;         [false true true] (true-false 0.2)
;         [true _ false] (true-false 0.2)
;         [false true false] (true-false 0.01)
;         [false false true] (true-false 0.1)
;         [false false false] (true-false 0.01)))
;
;(s/defn scholarship :- TrueFalseDistribution
;  [good-grades :- Boolean
;   high-sat :- Boolean]
;  (match [good-grades high-sat]
;         [true true] (true-false 0.9)
;         [true false] (true-false 0.4)
;         [false true] (true-false 0.4)
;         [false false] (true-false 0.0)))
;
;(s/defrecord AcademicsDistribution []
;  Distribution
;  (sample [this]
;          (let [s (.sample (smart))
;                a (.sample (affluent))
;                g (.sample (grades s))
;                sat (.sample (high-sat s a g))
;                sh (.sample (scholarship g sat))]
;            {:smart s :affluent a :grades g :high-sat sat :scholarship sh})))
;
;(s/defn academics [] (AcademicsDistribution.))

;; Another example from http://www.cs.ubc.ca/~murphyk/Bayes/bnintro.html

(def p-cloudy 0.5)

(s/defn p-sprinkler :- Double
  "Returns probability of sprinkler being on given whether it's cloudy or not."
  [cloudy :- Boolean]
  (get {true 0.1 false 0.5} cloudy))

(s/defn p-rain :- Double
  "Returns probability of it raining given whether it's cloudy or not."
  [cloudy :- Boolean]
  (get {true 0.8 false 0.2} cloudy))

(s/defn p-wet-grass :- Double
  "Returns probability of grass being wet given if the sprinkler is on/off and
  whether it's raining."
  [sprinkler :- Boolean
   rain :- Boolean]
  (match [sprinkler rain]
         [true true]    0.99
         [true false]   0.9
         [false true]   0.9
         [false false]  0))

(s/defn grass-map
  "Possible state of the Grass Bayesian network. For each distinct set of
  values [c s r w], we have a distinct state of the Bayesian network."
  [c :- Boolean s :- Boolean r :- Boolean w :- Boolean]
  {:cloudy c :sprinkler s :rain r :wet-grass w})

(s/defrecord GrassDistribution
  []
  PgmDistribution

  (state-prob
    [this state-map]
    (let [{:keys [cloudy sprinkler rain wet-grass]} state-map
          s1 (p-sprinkler cloudy)
          r1 (p-rain cloudy)
          wg (p-wet-grass sprinkler rain)
          pc (if cloudy p-cloudy (- 1 p-cloudy))
          ps (if sprinkler s1 (- 1 s1))
          pr (if rain r1 (- 1 r1))
          pw (if wet-grass wg (- 1 wg))]
      (apply * [pc ps pr pw])))

  (prob-of-states
    [this]
    (let [tf [true false]
          probs (atom {})]
      (doseq [cloudy tf
              sprinkler tf
              rain tf
              wet-grass tf]
        (let [state (grass-map cloudy sprinkler rain wet-grass)]
          (swap! probs assoc-in [state] (.state-prob this state))))
      (deref probs))))

(s/defn grass [] (GrassDistribution.))

