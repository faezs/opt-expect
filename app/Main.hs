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
import ConCat.AltCat ()
import ConCat.Rebox ()

import CartPole
import PPO
import Utils
import Env



main :: IO ()
main = reinforce



reinforce :: IO ()
reinforce = do
  putStrLn "running cartpole"
  let
    policyNet = (gaussInit <$> randF 1 :: PType 4 16 2)
  runEpochs (policyGradient 0.1) runCPEpisode initCP catAgent policyNet
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

--policyGradient 0.1

