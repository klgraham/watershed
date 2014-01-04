# Watershed

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
	* Fair coin
	* Biased coin
	* Pair of N-Sided Dice

## Installation

### Leiningen

Add the following to your `:dependencies`:

```clj
[watershed "0.1.0-SNAPSHOT"]
```

### Maven

```xml
<dependency>
  <groupId>watershed</groupId>
  <artifactId>watershed</artifactId>
  <version>0.1.0-SNAPSHOT</version>
</dependency>
```

## Usage

For now, please look at the examples in examples.clj. They should all be
straightforward. There are two PGM examples:

	1. the common "Rain" example
	2. the Student's example used in Daphne Kohler's PGM textbook

There are also examples of how to use this for basic probability stuff,
though for things like that it's perhaps better to use [Incanter](http://incanter.org).

## License

Copyright © 2013 Kenneth L. Graham

Distributed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html), same as Clojure.