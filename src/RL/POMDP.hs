{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module RL.POMDP where

import Prelude hiding ((.), id, uncurry, curry)

import ConCat.Synchronous
import ConCat.Misc
import ConCat.Category hiding (const)
import ConCat.Free.VectorSpace


-- A partially observable markov decision process
  
data POMDP (m :: * -> *) s a = POMDP (s -> a -> m s) (s -> m a) s -- -> POMDP m s a)
  --Action ::  POMDP m s a

-- A moore machine has the ability to run.

type POMDPCon m s a = (Trainable m, HasV R s, HasV R a)

-- Step Function
step :: forall m s a. (POMDPCon m s a) => POMDP m s a -> m (POMDP m s a)
step (POMDP sn an s) = return . (\s' -> POMDP sn an s') =<< (\a -> sn s a) =<< (an s) 

-- Objective
reward :: (s -> m R) -> POMDP m s a -> m R
reward f (POMDP _ _ s)= f s

class (Monad m) => Trainable m where
  -- State Model Gradient: We need a parameterization
  nextStateModel :: forall a s. R -> (s -> a -> m s) -> m (s -> a -> m s)
  -- Policy Gradient: We need a parameterization
  nextActionModel :: forall a s. R -> (s -> m a) -> m (s -> m a)


train :: (Trainable m) => POMDP m s a -> (s -> m R) -> m (POMDP m s a)
train (POMDP s' a' s) r =
   (r s) >>=
   (\r' -> POMDP <$> (nextStateModel r' s') <*> (nextActionModel r' a') <*> (pure s))


-- Semantic Function
pMDP :: (s -> a -> m s) -> (s -> m a) -> s -> POMDP m s a 
pMDP = POMDP
