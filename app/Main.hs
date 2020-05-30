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
import ConCat.Misc
import ConCat.Deep

import CartPole
import PPO
import Utils
import Env

import Control.Monad.Bayes.Sampler
import Streamly
import qualified Streamly.Prelude as S

main :: IO ()
main = reinforce



reinforce :: IO ()
reinforce = do
  putStrLn "running cartpole"
  initS <- initCPIO
  let
    policyNet = (gaussInit <$> randF 1 :: PType 4 16 2)
    valueNet = (gaussInit <$> randF 1 :: PType 4 16 1)
    --cpep :: IO CPEpisode
    --cpep = sampleIO $ runEpisode @SamplerIO @CPState @CPAct @R stepCP (catAgent policyNet) (wrapVF valueFn valueNet) initS
    --pl = S.scan (policyFold (policyGradient 0.1) policyNet) $ S.repeatM cpep
    --vf = S.scan (valueFnFold (valueFnLearn valueFn) valueNet) $ S.repeatM cpep
  --p' <- S.toList $ S.take 10 pl
  --vf' <- S.toList $ S.take 10 vf
  runEpochs 50 400 (policyGradient 3e-4) (valueFnLearn valueFn 1e-3) stepCP initCP catAgent valueFn policyNet valueNet
  return ()
{-# INLINE reinforce #-}


