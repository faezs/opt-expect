{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module CartPole where

import ConCat.Misc
import Control.Monad.State.Lazy
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler


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



data CPConf = CPConf
  { gravity :: R
  , massCart :: R
  , massPole :: R
  , totalMass :: R
  , poleHalfLength :: R
  , poleMassLength :: R
  , forceMag :: R
  , secondsBetweenUpdates :: R
  , thetaThreshold :: R
  , xThreshold :: R
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
  , thetaThreshold = 12
  , xThreshold = 2.4
  }


data CPState = CPState
  { x :: R  -- cart position
  , xdot :: R -- cart velocity
  , theta :: R -- pole angle
  , thetadot :: R -- pole velocity
  } deriving (Eq, Ord, Show)

data CPAct = PushLeft | PushRight deriving (Eq, Ord, Show)


type EnvState s r = State s r

data Transition s a r = Transition
  { s_t :: s
  , s_tn :: s
  , a_t :: a
  , rw :: r
  , done :: Bool
  } deriving (Eq, Ord, Show)

type CPTrans = Transition CPState CPAct R

stepCP :: CPConf -> (CPState -> CPAct) -> EnvState CPState CPTrans
stepCP c@CPConf{..} actor = do
  st <- get
  let
    act = actor st
    force = if act == PushRight then forceMag else (- forceMag)
    st' = delSt force st
    d = done st'
    r = reward
  put st'
  return $ Transition st st' act r d
  where
    reward :: R
    reward = 1
    done :: CPState -> Bool
    done CPState {..} = x < (- xThreshold)
      || x > (xThreshold)
      || theta < (- thetaThreshold)
      || theta > (thetaThreshold)
    delSt :: R -> CPState -> CPState
    delSt force s@CPState{..} = undefined
      where
        thetaAcc = (gravity * sinTh - cosTh * temp) / d 
          where
            d = poleHalfLength * ((4/3) - massPole * cosTh * cosTh / totalMass)
        xAcc = temp - poleMassLength * thetaAcc * cos theta / totalMass
        temp =  force + poleMassLength * thetadot * thetadot * sinTh / totalMass
        cosTh = cos theta
        sinTh = sin theta


runCP :: CPState -> (CPState -> CPAct) -> [CPTrans]
runCP initState actor = repeat $ evalState (stepCP cartPoleDef actor) initState

runEpisode :: (s -> (s -> a) -> [Transition s a r]) -> s -> (s -> a) -> [Transition s a r]
runEpisode r i a = (takeWhile (\Transition{done} -> done)) $ r i a

runCPEpisode :: CPState -> (CPState -> CPAct) -> [CPTrans]
runCPEpisode i a = runEpisode runCP i a

-- (State, Transition) -> 

initCP :: (MonadSample m) => m CPState
initCP = CPState <$> dist <*> dist <*> dist <*> dist
  where
    dist = uniform (-0.5) 0.5

initCPIO :: IO CPState
initCPIO = sampleIOfixed initCP

--type Agent m s a = StateT (s -> a) m 

dumbAgent :: CPState -> State Int CPAct
dumbAgent _ = do
  i <- get
  let
    a = if (mod i 2 == 0) then PushLeft else PushRight
  put (i + 1)
  return $ a

dA = evalState
