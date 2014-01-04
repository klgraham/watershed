(ns watershed.distribution_test
  (:use [midje.sweet]
        [watershed.distribution]))

(facts "Probability of rolling a 6-sided die and getting a 1-6 is one."
       (def die (discrete-uniform 1 6))
       (prob die (between? 1 6)) => 1.0)

(facts "Probability of rolling a pair of 6-sided dice and getting a 1-12 is one."
       ;(def die (dice 1 12))
       (prob (dice 1 6) (between? 1 12)) => 1.0)

(facts "Probability of getting heads when flipping a fair coin is about 1/2."
       (-> (prob (coin) (eq? 'H))
           (- 0.5)
           (Math/abs)
           (< 0.01)) => true)

(facts "Probability of getting heads when flipping an unfair coin with p=0.3 is 0.3."
       (-> (prob (biased-coin 0.3) (eq? 'H))
           (- 0.3)
           (Math/abs)
           (< 0.01)) => true)

(facts "Prob of std. normal values within 5 std. deviations is ~99.9%."
       (-> (prob (normal) (between? -5 5))
           (- 1.0)
           (Math/abs)
           (< 0.001)) => true)