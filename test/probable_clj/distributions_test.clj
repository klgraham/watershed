(ns probable-clj.distributions-test
  (:use [midje.sweet]
        [probable-clj.distributions]))

(facts "Sampling a from a distribution given a predicate should work."
      (> (sample-given #(> % 0.5) :uniform) 0.5) => true
      (> (sample-given #(> % 1.0) :gaussian) 1.0) => true
      (def t  (->> (given #(> % 0.5) :uniform 3)
                   (map #(> % 0.5))
                   (set)))
      t => #{true}
      )
