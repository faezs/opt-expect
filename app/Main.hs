{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

--{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}

--{-# OPTIONS_GHC -fsimpl-tick-factor=2500 #-}


module Main where

import Prelude hiding (zipWith, zip)
import GHC.TypeNats ( KnownNat )
import GHC.Generics (Par1(..),(:*:)(..),(:.:)(..))
import ConCat.AltCat (fromIntegralC)
import ConCat.Rebox ()
import ConCat.Misc ( R, (:*) )
import ConCat.Deep ( (^-^) )

import Env.CartPole ( initCP, stepCP, CPState, CPAct, CPTrans )
import RL.PPO ( catAgent, valueFn, pvFold )
import Utils.Utils ( V, HasV(toV), PType, gaussInit, randF )
import Env.Env
    ( Transition(Transition, rw, s_tn, a_t, advantage),
      minibatch,
      runEpisode,
      wrapVF, minibatchStatistics )
import Env.MonadEnv ( MonadEnv, sampleIOE )


import Streamly ( SerialT, adapt )
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import ConCat.RAD (gradR)
import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import qualified Streamly.Internal.Data.Stream.IsStream as S
import GHC.Float (expts10)


-- #define PROFILE

main :: IO ()
main = do
  putStrLn "running cartpole"
  reinforce



reinforce :: IO ()
reinforce = S.drain $ S.hoist sampleIOE $ S.iterateM epoch (pure (v, p))
  where
    p = gaussInit <$> randF 10123 :: PType 4 16 2
    v = gaussInit <$> randF 10123 :: PType 4 16 1
    epoch :: (PType 4 16 1, PType 4 16 2) -> MonadEnv ((PType 4 16 1, PType 4 16 2))
    epoch (v, p) = fmap fromJust
      $ S.fold FL.last
      $ S.postscan (pvFold 0.0001 (0.99*0.97) (v, p))
      $ S.tapAsync minibatchStatistics
      $ minibatch @128 (eps 512 p v)
{-# INLINE reinforce #-}


eps :: forall i j k o. (KnownNat i, KnownNat j, KnownNat k, KnownNat o, HasV CPState i, HasV CPAct o) => Int -> PType i j o -> PType i k 1 -> SerialT MonadEnv CPTrans
eps n pi vi = S.tap (FL.rmapM (liftIO . print . (" MeanReward: " <>) . show)
                     (FL.lmap rw FL.mean))
                    (runEpisode @MonadEnv stepCP (catAgent pi) (wrapVF valueFn vi) =<< S.replicateM n initCP)
{-# INLINE eps #-}



-- learnF nEpochs nEps (p, v) =
--   sampleIOE $ S.fold FL.last $ S.take nEpochs
--   $ (runPUpdate p &&& (runVUpdate
--                     $ minibatch @128 $ eps nEps ip iv (pure (p, v))
--   where
--     runPUpdate = S.iterateM (\ip -> S.fold ((\p v _ -> (p, v)) <$> pgFold ip <*> vfFold iv <*> minibatchStatistics)

-- pgFold pf = minibatchLearn @MonadEnv @128 @CPState @CPAct @4 @16 @2 10 (\b px -> policyGradient 1e-2 b px) pf
-- --{-# INLINE pgFold #-}

-- vfFold vf = minibatchLearn @MonadEnv @128 @CPState @CPAct @4 @16 @1 80 (\b px -> valueFnLearn (valueFn) 1e-2 b px) vf

--{-# INLINE learnF #-}


{--



--{-# INLINE vfFold #-}



--}
