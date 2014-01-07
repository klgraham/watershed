(ns watershed.examples
  (:import (java.lang Boolean))
  (:use [watershed.distribution]
        [watershed.predicates]))

;;;;; Contains examples of how to use probable-clj for:
;;;;;  * general probability distribution calculations
;;;;;  * probabilistic graphical models

;(def norm (normal))
;(println "Prob. of values between +/- 1: " (prob norm (between? -1 1)))
;(println "Prob. of values between +/- 2: " (prob norm (between? -2 2)))
;(println "Prob. of values between +/- 3: " (prob norm (between? -3 3)))
;(println "Prob. of values between +/- 4: " (prob norm (between? -4 4)))
;(println "Prob. of values between +/- 5: " (prob norm (between? -5 5)))
;
;(println "\nIf win prob. is 20%, what is the prob of a win in 10000 Bernoulli trials?")
;(println (prob (bernoulli 0.2) (eq? 1)))
;
;;; Rolling the dice: Composing distributions to obtain others
;;; 6-sided die
;(def die (discrete-uniform 1 6))
;(println "\nProb. of getting a 1, 2, or 3: " (prob die (between? 1 3)))
;
;;; This distribution is useful enough to build it in
;;; pair of 6-sided dice
;(def pair (dice 1 6))
;;(println "\nThere are three ways of rolling a 4: [2, 2], [1, 3], and [3, 1]")
;(println "Prob of rolling a 2: " (prob pair (eq? 2)))
;(println "Prob of rolling a 4: " (prob pair (eq? 4)))
;(println "Prob of rolling a 5: " (prob pair (eq? 5)))
;(println "Prob of rolling a 6: " (prob pair (eq? 6)))
;(println "Prob of rolling a 7: " (prob pair (eq? 7)))
;(println "Prob of rolling a 10: " (prob pair (eq? 10)))
;(println "Prob of rolling a 12: " (prob pair (eq? 12)))
;(println "Prob of rolling a 1-12: " (prob pair (between? 1 12)))
;
;;; Coin tosses
;(def fair-coin (coin))
;(println "\n10 rolls of a fair coin: " (flip fair-coin 10))
;(println "Frequency of heads (empirically): " (prob fair-coin (eq? 'H)))
;
;(def unfair-coin (biased-coin 0.3))
;(println "\n10 rolls of a unfair coin: " (flip unfair-coin 10))
;(println "Prob of heads (theoretically): " (:p unfair-coin))
;(println "Frequency of heads (empirically): " (prob unfair-coin (eq? 'H)))

;;; Probabilisic graphical model examples.
(println "\n********************")
(println "*** PGM examples ***")
(println "********************\n")

;;; Here is a schematic for the model. The notation is as follows:
;;; {:x [] :y [:x]} means:
;;; 1) x has no dependencies => P(x)
;;; 2) y depends on x => P(y|x)
;;;
;;; If this is not clear, then try this:
;;;
;;;         X -> Y

;(doall (map println (sample (academics) 5)))
;(println "What is the probability of getting a scholarship if grades are poor?")
;(println (prob (academics) #(= true (:scholarship %)) :given? #(= false (:grades %))))
;
;(println "What is the probability of getting a scholarship if not smart?")
;(println (prob (academics) #(= true (:scholarship %)) :given? #(= false (:smart %))))
;
;(println "What is the probability of getting a scholarship if not smart and have bad grades?")
;(println (prob (academics) #(= true (:scholarship %)) :given? #(and (= false (:grades %)) (= false (:smart %)))))
;
;(println "What is the probability of getting a scholarship if affluent?")
;(println (prob (academics) #(= true (:scholarship %)) :given? #(= true (:affluent %))))
;
;(println "What is the probability of getting a scholarship if grades are good but is poor?")
;(println (prob (academics) #(= true (:scholarship %)) :given? #(and (= false (:affluent %)) (= true (:grades %)))))
;
;(println "Traffic:\n")

;(doall (map println (sample (traffic) 5)))
;(def t (traffic))
;(println "1000 samples")
;(time (def s (.sample t)))
;(doall (map println s))
;(println "\n10000 samples")
;(time (def s (sample t 10)))
;(doall (map println s))
;(println "\n100000 samples")
;(time (def s (sample t 100)))
;(doall (map println s))
;(println "\n1,000,000 samples")
;(time (def s (sample t 1000)))
;(doall (map println s))
;(println "Prob of traffic jam given bad weather:")
;(time (println (prob t (tf? :traffic-jam true) :given? (tf? :bad-weather true) :debug true)))
;
;(println "Prob of traffic jam given bad weather and sirens:")
;(time (println (prob t (tf? :traffic-jam true)
;                     :given? (every-pred (tf? :bad-weather true) (tf? :sirens true)) :debug true)))
;
;(println "Prob of bad-weather given traffic jam:")
;(time (println (prob t (tf? :bad-weather true) :given (tf? :traffic-jam true) :debug true)))
;;(println (prob (traffic) (truth :traffic-jam) :given? (and (truth :bad-weather) (truth :sirens))))
;(println (prob (traffic) #(= true (:accident %)) :given? (and (truth :bad-weather) (truth :traffic-jam))))
;;(println (prob (traffic) #(= true (:accident %)) :given? (truth :traffic-jam)))
;;(println (traffic-dist 2))

;(println (prob (accident true) (eq? true)))
;(println (prob (traffic-jam true true false) (eq? true)))

;
(println "\n*** Rain example ***\n")
(def g (grass))
;(println (sample g 1))
;(doall (map println (metropolis-sampling g 10000)))
;(def s (sample g 10000))
;(doall (map println s))
;(println "Prob of a cloudy day: " (prob g (tf? :cloudy true)))
(println "Prob of wet grass: (0.6471)\n" (time (prob g (tf? :wet-grass true) :debug true)))
(println "Prob of wet grass given sprinkler on and no rain: (0.9)\n" (prob g (tf? :wet-grass true) :given? (every-pred (tf? :sprinkler true) (tf? :rain false))))
(println "Prob of dry grass given sprinkler on and no rain: (0.1)\n" (prob g (tf? :wet-grass false) :given? (every-pred (tf? :sprinkler true) (tf? :rain false))))
(println "Prob of rain given wet grass: (0.708)\n" (time (prob g (tf? :rain true) :given (tf? :wet-grass true) :debug true)))
(println "Prob of rain and wet grass: (0.4581)\n" (time (prob g (every-pred (tf? :rain true) (tf? :wet-grass true)) :debug true)))
(println "Prob of sprinkler given wet grass: (0.430)\n" (time (prob g (tf? :sprinkler true) :given? (tf? :wet-grass true) :debug true)))
(System/exit 0)