{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module CartPole where

import ConCat.Misc
import Control.Monad.State.Lazy
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler

import Control.Arrow
import Utils
import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)

import Env
{---------------------

A pole is attached by an unactuated joint to a cart, which moves along a frictionless track.

The pendulum starts upright, and the goal is to prevent it from falling over by increasing and reducing the cart's velocity.

 Source:
        This environment corresponds to the version of the cart-pole problem described by Barto, Sutton, and Anderson.
        Based on the implementation in www.github.com/openai/gym


Observation:
   A 4-vector with the following components over the specified intervals
   0) Cart Position:  (-4.8, 4.8)
   1) Cart Velocity (-inf, inf)
   2) Pole Angle (-24 deg, 24 deg)
   3) Pole Velocity At Tip (-inf, inf)

Actions: Two Discrete Actions
   0: Push cart to the left
   1: Push cart to the right.

Note: The amount the velocity that is reduced or increased is not fixed; it depends on the angle the pole is
        pointing. This is because the center of gravity of the pole increases the amount of energy needed to move the
        cart underneath it

Reward : 1 for every step taken, including termination step

Starting State: Uniform random value in [-0.05,..,0.05]

Termination Conditions:

Pole angle is more than 12 degrees
cart position is more than 2.4
episode length is greater than 200

Solved Requirements:
average reward >= 195 over 100 consecutive trials

----------------------}


-- This is what a transition trajectory looks like in CP
type CPTrans = Transition CPState CPAct R

type CPEpisode = Episode CPState CPAct R

data CPState = CPState
  { cpX:: !R  -- cart position
  , xdot :: !R -- cart velocity
  , theta :: !R -- pole angle
  , thetadot :: !R -- pole velocity
  } deriving (Eq, Ord, Show)



instance HasV CPState 4 where
  toV CPState{..} = fromJust $ VS.fromList @4 [cpX, xdot, theta, thetadot]
  fromV v = CPState{..}
    where cpX:xdot:theta:thetadot:[] = VS.toList v

data CPAct = PushLeft | PushRight deriving (Eq, Ord, Show, Enum)

instance HasV CPAct 2 where
  toV PushLeft = fromJust $ VS.fromList @2 [1, 0]
  toV PushRight = fromJust $ VS.fromList @2 [0, 1]
  fromV v = if x > y then PushLeft else PushRight
    where
      x:y:[] = VS.toList v

data CPConf = CPConf
  { gravity :: !R
  , massCart :: !R
  , massPole :: !R
  , totalMass :: !R
  , poleHalfLength :: !R
  , poleMassLength :: !R
  , forceMag :: !R
  , secondsBetweenUpdates :: !R
  , thetaThreshold :: !R
  , xThreshold :: !R
  } deriving (Eq, Ord, Show)

cartPoleDef = CPConf
  { gravity = 9.8
  , massCart = 1
  , massPole = 0.1
  , totalMass = (1 + 0.1)
  , poleHalfLength = 0.5
  , poleMassLength = (0.1 * 0.5)
  , forceMag = 10
  , secondsBetweenUpdates = 0.02
  , thetaThreshold = 24
  , xThreshold = 2.4
  }



toRad n = n * (pi / 180)

-- $ cartpole dynamics without friction
dynamicsCP :: CPConf -> R -> CPState -> CPState
dynamicsCP CPConf{..} f s@CPState{..} = CPState
      { cpX = cpX + (secondsBetweenUpdates * xdot)
      , xdot = xdot + (secondsBetweenUpdates * x'')
      , theta = theta + (secondsBetweenUpdates * thetadot)
      , thetadot = thetadot + (secondsBetweenUpdates * t'')
      }
      where
        tMass = massCart + massPole
        t'' = num / denom
          where
            num = (gravity * sinTh)
              + (cosTh *
                 (((- f)
                   -
                   (massPole * poleHalfLength * (thetadot * thetadot) * sinTh))
                  / tMass))
            denom = poleHalfLength * ((4/3) - ((massPole * (cos cosTh)) / tMass))
        x'' = ((f + (massPole * poleHalfLength * ((thetadot * thetadot * sinTh) - (t'' * cosTh)))) / tMass)
        cosTh = cos theta
        sinTh = sin theta


stepCP' :: forall m. (MonadSample m) => CPConf -> (CPState -> m CPAct) -> EnvState m CPState CPTrans
stepCP' c@CPConf{..} actor = do
  st <- get
  act <- lift $ actor st
  let
    force PushRight = forceMag
    force PushLeft = (- forceMag)
    st' = dynamicsCP c (force act) st
    d = done st'
    r = reward
  put st'
  return $ Transition st st' act r d 0 0
  where
    reward :: R
    reward = 1
    done :: CPState -> Bool
    done CPState {..} = cpX < (- xThreshold)
      || cpX > (xThreshold)
      || theta < (- (toRadians thetaThreshold))
      || theta > (toRadians thetaThreshold)


stepCP :: forall m. (MonadSample m) => (CPState -> m CPAct) -> EnvState m CPState CPTrans
stepCP = stepCP' cartPoleDef

runCPEpisode ::  CPState -> (CPState -> SamplerIO CPAct) -> (CPState -> R) -> SamplerIO (CPEpisode)
runCPEpisode i a v = runEpisode (stepCP) a v i


initCP :: (MonadSample m) => m CPState
initCP = CPState <$> dist <*> dist <*> dist <*> dist
  where
    dist = uniform (-0.5) 0.5

initCPIO :: IO CPState
initCPIO = sampleIO initCP


dumbAgent :: CPState -> State Int CPAct
dumbAgent _ = do
  i <- get
  let
    a = if (mod i 2 == 0) then PushLeft else PushRight
  put (i + 1)
  return $ a
