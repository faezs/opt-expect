{-# LANGUAGE DataKinds #-}

module SCG.Graph () where

import GHC.TypeLits
import Algebra.Graph
import Backprop
import Numeric.LinearAlgebra.Static

{--
Optimizing loss functions over random variables is often intractable
due to the loss functions and their gradients being either a sum over
an exponential number of latent variable configurations or non-analytic,
high-dimensional integrals.

Usually this is resolved by using problem-specific MC Grad Estimators.

There is a general technique for this!

Stochastic Computation Graphs:
- Allows derivation of unbiased gradient estimators for general expected losses.
- Estimator can be computed as the grad of a differentiable "surrogate loss" through backprop
- Variance reduction techniques can be applied to this problem formulation
- Hessian-free methods and majorization-minimization algorithms can be generalized to the SCG framework


!! The main modification to backprop is to introduce extra gradient signals at the stochastic nodes.
--}


-- Gradient Estimators for a Single Random Variable

type RV = Vector Double

type Theta = Matrix

parameterizedProbDist :: Theta -> RV

costfn :: RV -> Double
costfn rv = 2.0

-- scoreFunctionEstimator: By Log-Derivative Trick (valid if p_x_theta is continuous function of theta, though not necessarily for x)
-- also known as REINFORCE or likelihood ratio estimator
ddTheta_expect_over_x_f_x x f p theta = expectation $ x $ f x $ ddTheta $ log $ p x theta 


-- if x is deterministic differentiable (perhaps representable as a type constraint? find one)
-- function of theta and another rv z, (i.e, x(z, theta)) we can use
-- !!Pathwise Derivative
-- only valid if f(x(z, theta)) is continous function of theta for all z (another type constraint?)
ddTheta_expect_over_x_f_ztheta f x z theta = expectation $ z $ ddTheta $ f (x z theta)


-- Theta might appear inside the expectation and the prob dist!
-- ddtheta Expectation_z~p(., theta)[ f( x(z, theta) ) ]
-- then two terms in the gradient estimator:


ddtheta_expect_over_z_from_pOfTheta_f_of_x_of_z_theta p f x z theta =
  expectation map (\z -> pointwiseEstimate) sample (p z theta)
  where pointwiseEstimate = ddTheta $ f (x z theta) + (ddTheta $ log (p z theta)) * (f $ x z theta)



{--
1. SF can be used even if f is discontinuous or x is a discrete rv
2. SF only requires trajectories, PD requires f'(x)
3. SF has higher variance than PD, unless f is rough as in time-series problems with exploding gradients
4. PD has a deterministic limit, SF doesn't.
--}

{--
  STOCHASTIC COMPUTATION GRAPHS:
    Directed, Acyclic Graph with Three Kinds of nodes:
    - Input Nodes, set externally including the parameters we differentiate with respect to
    - Deterministic Nodes: pure functions of their parents
    - Stochastic Nodes: Distributed Conditionally on their parents.

    Each parent v of a non-input node w is connected to it by a directed edge (v, w)

  THE STRUCTURE OF THE GRAPH FULLY SPECIFIES:
    - What estimator we will use, SF or PD or a combination thereof
    - Nodes arranged in series are multiplicative terms only
    - Nodes arranged in parallel lead to sums over mulplicative terms
    - 
--}

data Node a = InputNode a | DeterministicNode a | StochasticNode a

newtype InputNode a = IO a

newtype DeterministicNode v e = ([v] -> e -> b)

newtype StochasticNode v e = ConditionalDistribution [v] e

data DirectedEdge a = DirectedEdge
  { _v :: Node,
    _w :: DeterministicNode | StochasticNode
  }



-- TODO: Create a directed acyclic graph DS using alga
-- TODO: Define Node types which typecheck (initially for a scalar parameterization of the relevant types)
-- TODO: Add backprop for folds over the deterministic nodes? derive an instance of Backprop for edges?
-- TODO: Add the right gradient estimation procedure for combinations of deterministic and stochastic nodes
-- TODO: Figure out how to do dataflow programming over the Graph
