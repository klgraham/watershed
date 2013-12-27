(ns probable-clj.examples
  (:require [probable-clj.distribution :as d]))

;;;;; Contains examples of how to use probable-clj for:
;;;;;  * general probability distribution calculations
;;;;;  * probabilistic graphical models

;;;; These examples use the code that doesn't just use functions,
;;;; but also uses protocols and factor functions

(def norm (d/normal))
(println "Sampling the Standard Normal distribution between +/-1:\n" (-> norm (.given (d/between? -1 1) 5)))
(println "Prob. of values between +/- 1: " (d/prob (.sample norm 10000) (d/between? -1 1)))
(println "Prob. of values between +/- 2: " (d/prob (.sample norm 10000) (d/between? -2 2)))
(println "Prob. of values between +/- 3: " (d/prob (.sample norm 10000) (d/between? -3 3)))
(println "Prob. of values between +/- 4: " (d/prob (.sample norm 10000) (d/between? -4 4)))
(println "Prob. of values between +/- 5: " (d/prob (.sample norm 10000) (d/between? -5 5)))