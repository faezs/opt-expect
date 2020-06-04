# optExpect

## For optimizing expectations

This is meant to be an attempt at building an RL framework which takes advantage of haskell's parallelization capabilities for running a large number of actors in parallel.

There are ongoing implementations of Vanilla Policy Gradients and Proximal Policy Optimization using primitives for feed-forward networks and AD provided by [concat](https://github.com/conal/concat). Both of these can be found in the PPO module.

Parametric Policies (gaussian and categorical) can be found in the Policy module. These use concat's very general formulation of parameters and utilize (monad-bayes)[https://github.com/tweag/monad-bayes#readme] for sampling and the calculation of probabilities.

There is also an abstraction for defining and running environments using (streamly)[https://github.com/composewell/streamly] for generating streams of trajectories and calculating generalized advantage estimates on them.

On the roadmap is an implementation of Stochastic Computational Graphs implemented perhaps using the (circuit abstraction)[https://github.com/conal/concat/blob/master/examples/src/ConCat/Circuit.hs] provided by concat.

## Running
Fairly straightforward if you have stack
 - stack build
 - stack run
