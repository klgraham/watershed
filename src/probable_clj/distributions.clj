(ns probable-clj.distributions
  (:import (java.util Random))
  (:require [schema.core :as s])
  (:use [clojure.set :only (difference)]))

;;; The idea here is to implement probability distributions in a way
;;; that will allow one to do probabilistic calculations in an almost
;;; natural language way. For example, to generate a bernoulli distribution
;;; of given type, you would use
;;;
;;; (distribution :bernoulli p),
;;;
;;; where p is the success probability. To compute conditional probability,
;;; there would be something like this:
;;;
;;; (prob traffic :traffic-jam :given :bad-weather),
;;;
;;; which means "What is the probability of the a trafic jam given bad weather?"

;;; Functions defined for each distribution
;;;
;;; This says what we can do with a distribution. The idea here is that a
;;; probability distribution isn't an object, but is really a function that
;;; returns certain types of values with certain statistical properties.
;;;
;;; So, we can do these things to a distribution:
;;; (1) sample a single value from it (return a single value)
;;; (2) sample multiple values from it (return a collection of values)
;;; (3) compute things like CDF, mean, and variance
;;; (4) query the distribution
;;; (5) compute probabilities
;;;
;;; The fourth type of operation, querying the distribution, is powerful and
;;; is what will enable the kind of question mentioned above to be asked.
;;; Queries will take the form of predicate functions, that essentially filter
;;; the values that can be returned by the distribution.

;;; Constants needed
(def MAX_INT java.lang.Integer/MAX_VALUE)
(def MAX_DOUBLE java.lang.Double/MAX_VALUE)
(def MIN_DOUBLE java.lang.Double/MIN_VALUE)

(def DistType (s/enum :int :long :uniform :normal :boolean :bernoulli
                      :exponential :pareto :discrete-uniform :chi2 :students-t)); :geometric))

;; Simple distributions, vis a vis how complex the code for generating them is
(def simple #{:int :long :uniform :normal :boolean :bernoulli
              :exponential :pareto :discrete-uniform :chi2 :students-t})

(defn bool->int [bool] (if bool 1 0))

(def default-dist-params
  {:int MAX_INT
   :exponential 1
   :pareto 1.0
   :boolean 0.5
   :bernoulli 0.5
   :geometric 0.5})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Probability Distributions

(s/defn sample
  "Draws one value from the specified probability distribution."
  [dist-type :- DistType
   & [parameter]]
  (let [r (new Random)
        dist-param (if (nil? parameter) (get default-dist-params dist-type 1) parameter)]
    (case dist-type
      :int (.nextInt r dist-param)
      :long (.nextLong r)
      :uniform (.nextDouble r)
      :normal (.nextGaussian r)

      ; produces true with probability dist-param
      :boolean (< (sample :uniform) dist-param)

      ; bernoulli random variable with success probability dist-param
      :bernoulli (if (sample :boolean dist-param) 1 0)

      :exponential (* (/ -1 dist-param) (Math/log (sample :uniform)))

      :pareto (* 1.0 (Math/pow (sample :uniform) (/ -1 dist-param)))

      ;:chi2 (let [dof dist-param
      ;            normal (repeatedly dof #(sample :normal))]
      ;        (->> (map #(* % %) normal)
      ;             (reduce +)))

      :students-t (let [k dist-param
                        z (sample :normal)
                        v (sample :chi2 k)
                        denom (Math/sqrt (/ v k))]
                    (/ z denom))

      ;; here, the parameter will be a vector of ints
      :discrete-uniform (rand-nth dist-param)
      ;:geometric (let [partial-bernoulli (partial dist :bernoulli 10000)
      ;                 bernoulli-list (repeatedly numberOfSamples #(partial-bernoulli dist-param))
      ;                 trials (map #(inc (.indexOf % 1)) bernoulli-list)
      ;                 g (map #(take %1 %2) trials bernoulli-list)]
      ;             (map #(dec (count %)) g))

      :else (throw (IllegalArgumentException. (str "Distribution type " dist-type " is not defined in sample"))))))

;(println "Gaussian: " (sample :normal))
;(println "Uniform: " (sample :uniform))

(s/defn sample-given
  "Returns a random variable from a given distribution that fulfill the predicate."
  [predicate
   dist-type :- DistType
   & [parameter]]
  (let [dist-param (if (nil? parameter) (get default-dist-params dist-type 1) parameter)
        x (sample dist-type dist-param)]
    (if (predicate x) x (sample-given predicate dist-type dist-param))))

;(println "uniform > 1/2: " (sample-given #(> % 0.5) :uniform))

(s/defn generate
  "Draws n i.i.d. random variables from the specified probability distribution."
  [sampler
   dist-type :- DistType
   numberOfSamples :- s/Int
   & [parameter]]
  (let [r (new Random)
        simple? (contains? simple dist-type)
        dist-param (if (nil? parameter) (get default-dist-params dist-type 1) parameter)]
    (if simple?
      (take numberOfSamples (repeatedly #(sampler dist-type dist-param)))
      (throw (IllegalArgumentException. (str "Distribution type " dist-type " is not defined in dist"))))
    ;(case dist-type
    ;
    ;  ;:geometric (let [partial-bernoulli (partial dist :bernoulli 10000)
    ;  ;                 bernoulli-list (repeatedly numberOfSamples #(partial-bernoulli dist-param))
    ;  ;                 trials (map #(inc (.indexOf % 1)) bernoulli-list)
    ;  ;                 g (map #(take %1 %2) trials bernoulli-list)]
    ;  ;             (map #(dec (count %)) g))
    ;
    ;  :else (throw (IllegalArgumentException. (str "Distribution type " dist-type " is not defined in dist"))))
    ))


(s/defn dist
  "Draws n i.i.d. random variables from the specified probability distribution."
  [dist-type :- DistType
   numberOfSamples :- s/Int
   & [parameter]]
  (let [dist-param (if (nil? parameter) (get default-dist-params dist-type 1) parameter)]
    (generate sample dist-type numberOfSamples dist-param)))

;(println "Uniform: " (dist :uniform 5))
;(println "Gaussian: " (dist :normal 5))

(s/defn given
  "Draws n i.i.d. random variables from the specified probability distribution that fulfill the predicate."
  [predicate
   dist-type :- DistType
   numberOfSamples :- s/Int
   & [parameter]]
  (let [sampler (partial sample-given predicate)
        dist-param (if (nil? parameter) (get default-dist-params dist-type 1) parameter)]
    (generate sampler dist-type numberOfSamples dist-param)))

;(println "Uniform and > 1/2: " (given #(> % 0.5) :uniform 5))
;(println "Gaussian and > 1: " (given #(> % 1.0) :normal 5))

(defn map-dist
  "Returns the distribution generated by applying the function
  fn to each value in the distribution. Note that the function can
  convert one distribution into another."
  [fn dist-seq] (map fn dist-seq))

(s/defn repeat-dist
  "Returns a collection of a collection of values drawn from a distribution.
  This is probably most useful for a discrete distribution."
  [dist-type :- DistType
   numberOfSamples :- s/Int
   & [parameter predicate]]
  (let [dist-param (if (nil? parameter) (get default-dist-params dist-type 1) parameter)
        sampler (if (nil? predicate)
                  (partial sample)
                  (partial sample-given predicate))]
    (repeatedly numberOfSamples #(generate sampler dist-type numberOfSamples dist-param))))

;(println "Repeat Uniform: " (repeat-dist :uniform 2))

(s/defn prob
  "Returns the probability that a random variable drawn from the sample
  distribution dist-sample obeys the predicate."
  [dist-seq
   predicate-fn]
  (-> (filter predicate-fn dist-seq)
      count
      (/ (.doubleValue (count dist-seq)))))

(defn between?
  "Probability that a random variable is between low and high"
  [low high]
  (fn [x] (and (>= x low)
               (<= x high))))

;;; Specific, novelty distributions:
;;;  6-sided die
;;;  pair of 6-sided dice
;;;  N-door Monty Hall problem (one prize)

(defn die-6 [n-rolls]
  (let [die (partial dist :discrete-uniform)
        sides (into #{} (range 1 7))]
    (-> n-rolls
        (die [1 6]))))

(defn dice [n-rolls]
  (let [[a b] [(vec (die-6 n-rolls)) (vec (die-6 n-rolls))]]
    (map + a b)))

;(println "6-sided die: " (die-6 10))
;(println (vec (die-6 5)))
;(println "two 6-sided dies: " (dice 10))

(defn monty-hall [num-doors]
  "N-door Monty Hall distribution.
  Return pairs of (door of prize, whether you switched"
  (let [doors (into [] (range 1 (inc num-doors)))
        prize-door (sample :discrete-uniform doors) ; prize placed behind a door at random
        chosen-door (sample :discrete-uniform doors) ; you choose one of the doors at random
        unselected-doors (vec (difference (set doors) #{prize-door} #{chosen-door}))
        opened-door (sample :discrete-uniform unselected-doors) ; Monty Hall chooses one of the other doors
        switch (sample :discrete-uniform (vec (difference (set doors) #{chosen-door} #{opened-door})))]
    [prize-door switch]))

;(def doors [1 2 3])
;(def prize-door (sample :discrete-uniform doors))
;(def chosen-door (sample :discrete-uniform doors))
;(println prize-door)
;(println chosen-door)
;(println (into #{} (.intValue prize-door)))
;(println (into #{} chosen-door))
;(def  unselected-doors (vec (difference (set doors)
;                                        (set prize-door)
;                                        (set chosen-door))))
;(def opened-door (sample :discrete-uniform unselected-doors))
;
;(println (monty-hall 3))
;(defn switching-wins? [[prize switch]] (= prize switch))
;(println "Prob. of winning Monty Hall Game by switching: " (prob (repeatedly 1000 #(monty-hall 3)) switching-wins?))
;(doall (println (map-dist #(* % %) (dist :int 20 3))))
;(doall (println (map-dist #(< % 0) (dist :normal 20))))

;(s/defn prob
;  "Returns the probability that a random variable drawn from the sample
;  distribution dist-sample obeys the predicate."
;  [dist-type :- DistType
;   predicate-fn
;   numberOfSamples :- s/Int]
;  (-> (filter predicate-fn (dist dist-type numberOfSamples))
;      count
;      (/ (.doubleValue numberOfSamples))))
;;
;(s/defn prob-d
;  "Returns the probability that a random variable drawn from the sample
;  distribution dist-sample obeys the predicate. Uses 10,000 samples."
;  [dist-type :- DistType
;   predicate-fn]
;  (prob dist-type predicate-fn 10000))



;(println "Prob of rolling a 4 on a 6-sided die: " (prob (die-6 10000) #(= % 4)))
;(println "Prob of rolling a 4 on a pair of 6-sided dice: " (prob (dice 10000) #(= % 4)))

;(s/defn geometric
;  [p :- s/Number
;   numberOfSamples :- s/Int]
;  (let [partial-bernoulli (partial dist :bernoulli 10000)
;        bernoulli-list (repeatedly numberOfSamples #(partial-bernoulli p))
;        trials (map #(inc (.indexOf % 1)) bernoulli-list)
;        g (map #(take %1 %2) trials bernoulli-list)]
;    (map #(dec (count %)) g)))

;(s/defn sample-chi-squared-once
;  "Samples a single random variable from the chi-squared distribution."
;  [dof :- s/Int]
;  (let [g (dist :normal dof)
;        g-sqr (map #(* % %) g)]
;    (reduce + g-sqr)))
;
;(s/defn chi-squared
;  [dof :- s/Int
;   numberOfSamples :- s/Int]
;  (let [chi-sqr #(sample-chi-squared-once %)]
;    (repeatedly numberOfSamples #(chi-sqr dof)))
;  )

;(println "\nTwo Gaussians")
;(def two-gaussians (my-repeat 2 :normal 3))
;(doseq [d two-gaussians]
;  (->> d (map println) doall))
;
;(println "\nGeometric 0.5")
;(->> (geometric 0.5 5) (map println) doall)
;
;(println "\nGaussian")
;;(println (uniform-dist :int 5))
;;(println (uniform-dist :uniform 5))
;(s/with-fn-validation (->> (dist :normal 5) (map println) doall))
;
;(println "\nTrue/False")
;(s/with-fn-validation (->> (dist :boolean 10 0.7) (map println) doall))
;
;(println "\nPareto")
;(s/with-fn-validation (->> (dist :pareto 10) (map println) doall))
;;(println (nonuniform-dist :normal 5))
;;(println (uniform-dist :float 5))
;;(println (uniform-dist :poisson 5))
;
;;(->> (uniform-dist :uniform 5) (map #(* 2 %)) (map println) doall)
;
;;(println (uniform-dist :boolean 5))
;;(println (map #(if (%) 1 0) (uniform-dist :boolean 5)))
;(println "\nBernoulli")
;(s/with-fn-validation (->> (dist :bernoulli 5) (map println) doall))


;(println "\nBoolean, with p = 0.75")
;(def tf (dist :boolean 10000 0.75))
;(println (prob-dist tf (fn [x] (= x true))))
;
;(println "\nBernoulli, with p = 0.75")
;(def bernoulli (dist :bernoulli 10000 0.75))
;(println (prob-dist bernoulli (fn [x] (= x 1))))
;

;(println "\nGeometric1, with p = 0.5")
;(def g1 (dist :geometric 10000 0.5))
;(println (str "Prob of Heads on 1 flip: " (prob-given g1 (fn [x] (= x 0)))))
;(println (str "Prob of Heads on 2 flips: " (prob-given g1 (fn [x] (= x 1)))))
;(println (str "Prob of Heads on 3 flips: " (prob-given g1 (fn [x] (= x 2)))))
;(doall (map println (dist :geometric 10 0.5)))

;(println "\nGaussian, prob withing 2 stddev")
;(println (prob-d :normal (within? 2)))
;(println "\nGaussian, prob between [-2,2]")
;(println (prob-d :normal (between? -2 2)))

;(def std-normal (dist :normal 100000))
;(println "Prob of Std. Normal variable between -1 and 1: " (prob-given std-normal (between? -1 1)))

;(println "\nGaussian random vars above 0")
;(println (dist-given :normal 10 (fn [x] (> x 0))))

;(def die (filter
;           (fn [x] (and (>= x 1) (<= x 6)))
;           (dist :int 200000 7)))

;(println "\n6-sided die, prob of 4.")
;(println (prob-d :int (fn [x] (and (>= x 1) (<= x 6)))))
;(println (prob-dist die (fn [x] (= x 4))))



;; Visualizing distributions

;(defn histogram
;  "Visualization of Histogram of given distribution"
;  [dist bins]
;  (let [hist (atom [])
;        max (last bins)]
;    (doseq [bin bins]
;      (swap! hist assoc-in [bin] 0))
;    (swap! hist assoc-in [(inc max)] 0)
;    (doseq [r dist]
;      (let [floor (Math/floor r)
;            index (if (> floor max) (inc max) floor)]
;        (swap! hist assoc-in [index] (fnil inc 0))))))
;
;(def bins [0 1 2 3 4 5 6])
;(def die-hist (histogram die bins))
;(def die-histogram (zipmap bins die-hist))
;(println die-histogram)
