(ns watershed.predicates
  (:require [schema.core :as s]))

;;;;; Predicates used to evaluate probabilities

;; Useful predicates
(s/defn gt? "Is x greater than y?"
  [y :- s/Number]
  (fn [x] (> x y)))

(s/defn gteq? "Is x >= y?"
  [y :- s/Number]
  (fn [x] (>= x y)))

(s/defn lt? "Is x < y?"
  [y :- s/Number]
  (fn [x] (< x y)))

(s/defn lteq? "Is x <= to y?"
  [y :- s/Number]
  (fn [x] (<= x y)))

(s/defn between?
  "Is x in [low, high]"
  [low :- s/Number
   high :- s/Number]
  (fn [x] (and (>= x low)
               (<= x high))))

(s/defn eq? [y :- s/Number] (fn [x] (= x y)))

(s/defn tf?
  "Returns a function that will operate on a hash-map/tuple where the key is itself
  a hash-map and return true if the key-of-interest has value true. Used for
  PGM-type distributions."
  [key-of-interest :- s/Keyword
   tf :- Boolean]
  (fn [map] (= tf (get (get map 0) key-of-interest false))))
