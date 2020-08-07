{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}

--{-# OPTIONS_GHC -fsimpl-tick-factor=2500 #-}


module Main where

import Prelude hiding (zipWith, zip)
import GHC.Generics (Par1(..),(:*:)(..),(:.:)(..))
import ConCat.AltCat (fromIntegralC)
import ConCat.Rebox ()
import ConCat.Misc
import ConCat.Deep

import Env.CartPole
import RL.PPO
import Utils.Utils
import Env.Env
import Env.MonadEnv

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

import Control.Monad.IO.Class
import ConCat.RAD (gradR)
import ConCat.Additive
import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import GHC.TypeLits
import Data.Key
import Data.Proxy
import qualified Data.Vector.Sized as VS

-- #define PROFILE

main :: IO ()
main = reinforce



reinforce :: IO ()
reinforce = do
  putStrLn "running cartpole"
  let
    policyNet = (gaussInit <$> randF 1 :: PType 4 16 2)
    valueNet = (randF 1 :: PType 4 16 1)
  traj <- sampleIOE $ S.head $ (minibatch @20) (eps 10 policyNet valueNet)
  let
   p'' = p' policyNet (preprocTx . VS.head . fromJust $ traj)
  print p''
  return ()
{-# INLINE reinforce #-}

p' = \p (s, a, r) -> gradR (\p' -> ppoLoss 0.2 s a r p') p
{-# INLINE p' #-}


eps :: Int -> PType 4 16 2 -> PType 4 16 1 -> SerialT MonadEnv (CPTrans)
eps = \n pi vi -> 
  (\s -> runEpisode @MonadEnv stepCP (catAgent pi) (wrapVF valueFn vi) s) =<< (S.replicateM n initCP)
{-# INLINE eps #-}

preprocTx = undefined

--actor :: (KnownNat3 i h o) => Unop (PType i h o) 
{--
#ifdef PROFILE
pgFold pf = minibatchLearn @MonadEnv @40 @CPState @CPAct @4 @16 @2 20 (\b px -> policyGradient 1e-2 b px) pf --policyGradient 1e-2 b px) pf
--{-# INLINE pgFold #-}

vfFold vf = minibatchLearn @MonadEnv @40 @CPState @CPAct @4 @16 @1 20 (\b px -> id px) vf --valueFnLearn (valueFn) 1e-2 b px) vf
--{-# INLINE vfFold #-}

#else

pgFold pf = minibatchLearn @MonadEnv @128 @CPState @CPAct @4 @16 @2 10 (\b px -> policyGradient 1e-2 b px) pf
--{-# INLINE pgFold #-}

vfFold vf = minibatchLearn @MonadEnv @128 @CPState @CPAct @4 @16 @1 80 (\b px -> valueFnLearn (valueFn) 1e-2 b px) vf
--{-# INLINE vfFold #-}
#endif

learnF nEpochs nEps (p, v) =
  sampleIOE $ S.fold FL.last $ S.take nEpochs
  $ (runPUpdate p &&& runVUpdate)
                    $ minibatch @128 $ eps nEps ip iv) (pure (p, v))
  where
    runPUpdate = S.iterateM (\ip -> S.fold ((\p v _ -> (p, v)) <$> pgFold ip <*> vfFold iv <*> minibatchStatistics)
--{-# INLINE learnF #-}
--}
