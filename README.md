# optExpect

## For optimizing expectations

This is meant to be an attempt at building an RL framework which takes advantage of haskell's parallelization capabilities for running a large number of actors in parallel.

There are ongoing implementations of Vanilla Policy Gradients and Proximal Policy Optimization using primitives for feed-forward networks and AD provided by [concat](https://github.com/conal/concat). Both of these can be found in the [PPO module](src/PPO.hs).

Parametric Policies (gaussian and categorical) can be found in the [Policy module](src/Policy.hs). These use concat's very general formulation of parameters and utilize [monad-bayes](https://github.com/tweag/monad-bayes#readme) for sampling and the calculation of probabilities.

SGD is currently how concat does it, but there's also work ongoing on an implementation of [ADAM](https://arxiv.org/abs/1412.6980) so optimization works faster. This can be found [here](src/Adam.hs)

There is also an abstraction for defining and running environments using [streamly](https://github.com/composewell/streamly) for generating streams of trajectories and calculating generalized advantage estimates on them in [Env](src/Env.hs).

## Future Work

On the roadmap is an implementation of Stochastic Computational Graphs implemented perhaps using the [circuit abstraction](https://github.com/conal/concat/blob/master/examples/src/ConCat/Circuit.hs) provided by concat.
Execution on GPUs, whether by switching out the NN stuff for hasktorch or haskell/tensorflow or by figuring out (or more likely waiting for) how conal means to implement compilation to GPUs with concat.

## Running
Fairly straightforward if you have stack
 - stack build
 - stack run
