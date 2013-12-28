(ns probable-clj.examples
  (:use [probable-clj.distribution]))

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
(println "Only generate samples with values > 1.5" (-> norm (given (gt? 1.5) 10)))

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

;;; generating multiple sets of samples of a distribution
;(def five-coins (repeatedly 5 #(.flip fair-coin 5)))
;(println "5 fair coins:" five-coins)
;(doall (map println five-coins))
;(doall (mapcat #(into [] %) five-coins))