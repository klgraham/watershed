(ns probable-clj.examples
  (:use [probable-clj.distribution]
        [clojure.core.match :only (match)])
  (:require [schema.core :as s]))

;todo: See how to import a record from a different ns that is defined with s/defn
;;;;; Contains examples of how to use probable-clj for:
;;;;;  * general probability distribution calculations
;;;;;  * probabilistic graphical models

(def norm (normal))
(println "Sampling the Standard Normal distribution between +/-1:\n" (-> norm (given (between? -1 1) 5)))
(println "Prob. of values between +/- 1: " (prob norm (between? -1 1)))
(println "Prob. of values between +/- 2: " (prob norm (between? -2 2)))
(println "Prob. of values between +/- 3: " (prob norm (between? -3 3)))
(println "Prob. of values between +/- 4: " (prob norm (between? -4 4)))
(println "Prob. of values between +/- 5: " (prob norm (between? -5 5)))
(println "Only generate samples with values > 1.5" (-> norm (given (gt? 1.5) 5)))

(println "\nIf win prob. is 20%, what is the prob of a win in 10000 Bernoulli trials?")
(println (prob (bernoulli 0.2) (eq? 1)))
(println "\nWhat about in 1000 Bernoulli trials?")
(println (prob (bernoulli 0.2) (eq? 1) 1000))

;; Rolling the dice: Composing distributions to obtain others
;; 6-sided die
(def die (discrete-uniform 1 6))
(println "\nProb. of getting a 1, 2, or 3: " (prob die (between? 1 3)))

;; This distribution is useful enough to build it in
;; pair of 6-sided dice
(def pair (dice 1 6))
;(println "\nThere are three ways of rolling a 4: [2, 2], [1, 3], and [3, 1]")
(println "Prob of rolling a 2: " (prob pair (eq? 2)))
(println "Prob of rolling a 4: " (prob pair (eq? 4)))
(println "Prob of rolling a 5: " (prob pair (eq? 5)))
(println "Prob of rolling a 6: " (prob pair (eq? 6)))
(println "Prob of rolling a 7: " (prob pair (eq? 7)))
(println "Prob of rolling a 10: " (prob pair (eq? 10)))
(println "Prob of rolling a 12: " (prob pair (eq? 12)))
(println "Prob of rolling a 1-12: " (prob pair (between? 1 12)))

;; Coin tosses
(def fair-coin (coin))
(println "\n10 rolls of a fair coin: " (flip fair-coin 10))
(println "Frequency of heads (empirically): " (prob fair-coin (eq? 'H)))

(def unfair-coin (biased-coin 0.3))
(println "\n10 rolls of a unfair coin: " (flip unfair-coin 10))
(println "Prob of heads (theoretically): " (:p unfair-coin))
(println "Frequency of heads (empirically): " (prob unfair-coin (eq? 'H)))

;;; Probabilisic graphical model test. We'll use the same example as in
;;; http://jliszka.github.io/2013/12/18/bayesian-networks-and-causality.html

;; constants for priors
(def p-rush 0.2)
(def p-weather 0.05)
(def p-accident-bad-weather 0.3)
(def p-accident-good-weather 0.1)
(def p-sirens-accident 0.9)
(def p-sirens-no-accident 0.2)
(def p-rwa 0.9)

;; priors
(def rush-hour (true-false p-rush))
(def bad-weather (true-false p-weather))

;; conditionals
(s/defn accident
  [weather-is-bad :- Boolean]
  (if weather-is-bad
    (true-false p-accident-bad-weather)
    (true-false p-accident-good-weather)))

(s/defn sirens
  [accident :- Boolean]
  (if accident
    (true-false p-sirens-accident)
    (true-false p-sirens-no-accident)))

(s/defn traffic-jam
  [rush-hour bad-weather accident]
  (match [rush-hour bad-weather accident]
         [true true _] (true-false p-rwa)
         [true _ true] (true-false p-rwa)
         [_ true true] (true-false p-rwa)
         [true false false] (true-false 0.5)
         [false true false] (true-false 0.3)
         [false false false] (true-false 0.6)
         [false false false]  (true-false 0.1)))

;(println "accident   : " (sample accident 10))
;(println "sirens     : " (sample sirens 10))
;(println "traffic jam: " (sample traffic 10))

(s/defrecord TrafficJam
             [rush-hour :- Boolean
              bad-weather :- Boolean
              accident :- Boolean
              sirens :- Boolean
              traffic-jam :- Boolean])

;; todo: cannot currently compose distributions in a straightforward way. fixme
;(defn traffic
;  []
;  (loop [r rush-hour
;         w bad-weather
;         a accident
;         s sirens
;         t traffic-jam
;         table []]
;    (if (and (empty? r) (empty? w))
;      table
;      (recur (rest r) (rest w) (rest a) (rest s) (rest t)
;             (conj table (TrafficJam. (first r) (first w) (first a) (first s) (first t)))))))
