{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

--{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}

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
import qualified Streamly.Internal.Data.Fold as FL

import Control.Monad.IO.Class
import ConCat.RAD (gradR)
import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)


-- #define PROFILE

main :: IO ()
main = reinforce



reinforce :: IO ()
reinforce = do
  putStrLn "running cartpole"
  let
    policyNet = (gaussInit <$> randF 1 :: PType 4 16 2)
    valueNet = (randF 1 :: PType 4 16 1)
  p'' <- sampleIOE $ S.toList $ S.iterateM (\(policy, vf) -> do
                                               let txs = (eps 30 policy vf)
                                               p' <- ppoUpdate @30 0.001 (0.99*0.97) txs policy
                                               vf' <- S.foldl' (\vf tx -> valueFnLearn @30 valueFn 0.99 tx valueNet) vf (minibatch txs)
                                               return (p', vf')
                                           ) (pure (policyNet, valueNet))
  mapM (\p -> print $ (fst p) ^-^ policyNet) p''
  return ()
{-# INLINE reinforce #-}


eps :: Int -> PType 4 16 2 -> PType 4 16 1 -> SerialT MonadEnv (CPTrans)
eps = \n pi vi -> S.tap (FL.mapM (liftIO . print) (FL.lmap rw FL.maximum))
  ((\s -> runEpisode @MonadEnv stepCP (catAgent pi) (wrapVF valueFn vi) s) =<< (S.replicateM n initCP))
{-# INLINE eps #-}


preprocTx :: CPTrans -> (V 4 R, V 2 R, R) 
preprocTx (Transition{ s_tn, a_t, advantage}) = (toV s_tn, toV a_t, advantage) 


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
