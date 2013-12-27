(ns probable-clj.examples
  (:require [probable-clj.distribution :as d]))

;;;;; Contains examples of how to use probable-clj for:
;;;;;  * general probability distribution calculations
;;;;;  * probabilistic graphical models

;;;; These examples use the code that doesn't just use functions,
;;;; but also uses protocols and factor functions

(def norm (d/normal))
(println "Sampling the Standard Normal distribution between +/-1:\n" (-> norm (.given (d/between? -1 1) 5)))
(println "Prob. of values between +/- 1: " (d/prob norm (d/between? -1 1)))
(println "Prob. of values between +/- 2: " (d/prob norm (d/between? -2 2)))
(println "Prob. of values between +/- 3: " (d/prob norm (d/between? -3 3)))
(println "Prob. of values between +/- 4: " (d/prob norm (d/between? -4 4)))
(println "Prob. of values between +/- 5: " (d/prob norm (d/between? -5 5)))

(println "\nIf win prob. is 20%, what is the prob of a win in 10000 Bernoulli trials?")
(println (d/prob (d/bernoulli 0.2) (d/eq? 1)))
(println "\nWhat about in 1000 Bernoulli trials?")
(println (d/prob (d/bernoulli 0.2) (d/eq? 1) 1000))

;; Rolling the dice: Composing distributions to obtain others
