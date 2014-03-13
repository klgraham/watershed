(ns watershed.distribution_test
  (:use [midje.sweet]
        [watershed.distribution]
        [watershed.predicates]
        [taoensso.timbre :only (info)]))

(facts "About rolling dice."
       (fact "Probability of rolling a 6-sided die and getting a 1-6 is one."
              (def die (discrete-uniform 1 6))
              (prob die (between? 1 6)) => 1.0)

       (fact "Probability of rolling a pair of 6-sided dice and getting a 1-12 is one."
              ;(def die (dice 1 12))
              (prob (dice 1 6) (between? 1 12)) => 1.0))

(facts "About a coin toss."
       (fact "Probability of getting heads when flipping a fair coin is about 1/2."
             (let [p-heads (-> (prob (coin) (eq? 'H)))]
               (info "Fair-coin toss: Probability of Heads = " p-heads)
               (-> (- p-heads 0.5)
                   (Math/abs)
                   (< 0.01))) => true)

       (fact "Probability of getting heads when flipping an unfair coin with p=0.3 is 0.3."
             (let [p-heads (-> (prob (biased-coin 0.3) (eq? 'H)))]
               (info "Biased-coin (0.3) toss: Probability of Heads = " p-heads)
               (-> (- p-heads 0.3)
                   (Math/abs)
                   (< 0.01))) => true ))

(fact "Prob of std. normal values within 5 std. deviations is ~99.9%."
       (-> (prob (normal) (between? -5 5))
           (- 1.0)
           (Math/abs)
           (< 0.001)) => true)

(fact "Sum of probabilities of all states is unity."
      (let [g (grass)
            num-samples 10000]
        (-> (reduce + (vals (sample g num-samples)))
            dec
            (Math/abs)
            (< 0.00001))) => true)