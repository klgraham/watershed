# Watershed

[![Build Status](https://travis-ci.org/klgraham/watershed.svg?branch=master)](https://travis-ci.org/klgraham/watershed)

Watershed provides a framework for working with Probabilistic Graphical
Models (PGMs) in Clojure. It's designed with experimentation and learning in
mind, so you can quickly define your own PGM and perform queries. At
present, this is setup mainly for analyzing Bayesian Networks, which are also
Directed Acyclic Graphs.

Probability distributions currently included:

	* Uniform
	* Standard Normal
	* Boolean (true/false)
	* Bernoulli
	* Exponential
	* Discrete Uniform
	* Poisson
	* Binomial
	* Fair coin
	* Biased coin
	* Pair of N-Sided Dice

Bayesian Networks currently included:

	* Rain example

Planned updates:

    * Convert all basic distribution code to call Incanter functions. No need to reinvent the wheel.
    * Add Bayesian Networks:
      * Student example from Koller & Friedman's PGM textbook


## Installation

### Leiningen

Add the following to your `:dependencies` in `project.clj`:

```clj
[watershed "0.2.0-SNAPSHOT"]
```

### Maven

```xml
<dependency>
  <groupId>watershed</groupId>
  <artifactId>watershed</artifactId>
  <version>0.2.0-SNAPSHOT</version>
</dependency>
```

## Usage

For now, please look at the examples in examples.clj. They should all be
straightforward. There are two PGM examples:

	1. the common "Rain" example
	2. the Student's example


## License

Copyright © 2015 Kenneth Graham

Distributed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html), same as Clojure.
