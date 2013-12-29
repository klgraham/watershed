(ns probable-clj.examples
  (:import (java.lang Boolean))
  (:use [probable-clj.distribution])
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
(println (prob (bernoulli 0.2) (eq? 1)) :samples 10000)

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

;;; Probabilisic graphical model examples.
(println "\n*******************")
(println "*** PGM examples ***")
(println "*******************\n")

;;; Here is a schematic for the model. The notation is as follows:
;;; {:x [] :y [:x]} means:
;;; 1) x has no dependencies => P(x)
;;; 2) y depends on x => P(y|x)
;;;
;;; If this is not clear, then try this:
;;;
;;;         X -> Y

;(doall (map println (sample (academics) 5)))
(println "What is the probability of getting a scholarship if grades are poor?")
(println (prob (academics) #(= true (:scholarship %)) :given? #(= false (:grades %))))

(println "What is the probability of getting a scholarship if not smart?")
(println (prob (academics) #(= true (:scholarship %)) :given? #(= false (:smart %))))

(println "What is the probability of getting a scholarship if not smart and have bad grades?")
(println (prob (academics) #(= true (:scholarship %)) :given? #(and (= false (:grades %)) (= false (:smart %)))))

(println "What is the probability of getting a scholarship if affluent?")
(println (prob (academics) #(= true (:scholarship %)) :given? #(= true (:affluent %))))

(println "What is the probability of getting a scholarship if grades are good but is poor?")
(println (prob (academics) #(= true (:scholarship %)) :given? #(and (= false (:affluent %)) (= true (:grades %)))))

(println "\n*** Traffic Jam example ***\n")
(s/defn truth [k :- s/required-key]
  (fn [x] (= true (k x))))

(println (prob (traffic) (truth :traffic-jam) :given? (truth :bad-weather) :samples 100000))
;(println (prob (traffic) (truth :traffic-jam) :given? (and (truth :bad-weather) (truth :sirens)) :samples 10000))
(println (prob (traffic) #(= true (:accident %)) :given? (and (truth :bad-weather) (truth :traffic-jam)) :samples 10000))
;(println (prob (traffic) #(= true (:accident %)) :given? (truth :traffic-jam) :samples 10000))
;(println (traffic-dist 2))
;;(println (let [d (filter #() (traffic-dist 10))]))