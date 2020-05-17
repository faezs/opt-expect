{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}
--{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}

--{-# OPTIONS_GHC -fsimpl-tick-factor=2500 #-}


module Main where

import GHC.Generics (Par1(..),(:*:)(..),(:.:)(..))
import ConCat.AltCat ((&&&), fork)
import ConCat.Rebox ()
import ConCat.RAD (gradR)
import ConCat.Misc
import ConCat.Deep
import CartPole
import PPO
import Utils
import Env

import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import ConCat.Additive

import Control.Monad
import Control.Monad.Bayes.Sampler
import GHC.TypeLits

import qualified Streamly.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL

main :: IO ()
main = reinforce

reinforce :: IO ()
reinforce = do
  putStrLn "running cartpole"
  let
    policyNet = (gaussInit <$> randF 1 :: PType 4 16 2)
    --valueNet = (gaussInit <$> randF 1 :: PType 4 16 1)
    --es = rollOut runCPEpisode initS policyNet
    --sts = fmap (toV @CPState @4 . s_t) es
    --acts = fmap (toV @CPAct @2 . a_t) es
    --a = (pgUpdate 0.1 es) policyNet
    --runner :: PType 4 16 2 -> IO (PType 4 16 2)
    --runner initPs = pgCpEp initS initPs
  runCartPole 0 policyNet
{-# INLINE reinforce #-}

{--
pgCpEp :: CPState -> PType 4 32 2 -> IO (PType 4 32 2)
pgCpEp = \initS startPs -> fst <$> (fork ((liftM $ flip learner $ startPs), fmap print)) $ envEp initS (agent startPs) --agentEpisode learner agent envEp steps
  where
    --learner :: CPEpisode -> Unop (PType 4 16 2)
    learner = policyGradient 0.1
    --agent :: PType 4 16 2 -> CPState -> SamplerIO CPAct
    agent = catAgent
    --envEp :: CPState -> (CPState -> SamplerIO CPAct) -> IO (CPEpisode)
    envEp = runCPEpisode
    --steps :: Int
    steps = 1000
{-# INLINE pgCpEp #-}
--}

runCartPole :: Int -> PType 4 16 2 -> IO ()
runCartPole epoch p = do
    s <- initCPIO
    let nEps = 20 :: Int
    episodes <- S.fold FL.mconcat $ parallely $ S.replicateM nEps (runCPEpisode s (catAgent p))
    print $ ("epoch: " <> (show epoch) <> ", average reward: " <> (show (reward episodes / fromIntegral nEps)))
    p' <- return $ policyGradient 0.1 episodes p
    runCartPole (epoch + 1) p'
{-# INLINE runCartPole #-}
