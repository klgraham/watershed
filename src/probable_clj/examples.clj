(ns probable-clj.examples
  (:use [probable-clj.distribution]))

;;;;; Contains examples of how to use probable-clj for:
;;;;;  * general probability distribution calculations
;;;;;  * probabilistic graphical models

;;;; These examples use the code that doesn't just use functions,
;;;; but also uses protocols and factor functions

(def norm (normal))
(println "Sampling the Standard Normal distribution between +/-1:\n" (-> norm (.given (between? -1 1) 5)))
(println "Prob. of values between +/- 1: " (prob norm (between? -1 1)))
(println "Prob. of values between +/- 2: " (prob norm (between? -2 2)))
(println "Prob. of values between +/- 3: " (prob norm (between? -3 3)))
(println "Prob. of values between +/- 4: " (prob norm (between? -4 4)))
(println "Prob. of values between +/- 5: " (prob norm (between? -5 5)))

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