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
import ConCat.RAD (gradR)
import ConCat.Misc
import ConCat.Deep
import CartPole
import PPO
import Utils
import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import ConCat.Additive

main :: IO ()
main = reinforce

reinforce :: IO ()
reinforce = do
  initS <- initCPIO
  putStrLn "running cartpole"
  let
    policyNet = (gaussInit <$> randF 1 :: PType 4 16 2)
    --valueNet = (gaussInit <$> randF 1 :: PType 4 16 1)
    --es = rollOut runCPEpisode initS policyNet
    --sts = fmap (toV @CPState @4 . s_t) es
    --acts = fmap (toV @CPAct @2 . a_t) es
    --a = (pgUpdate 0.1 es) policyNet
  (putStrLn . show . length) $ runCPEpisode initS $ agent $ last $ runEps runCPEpisode 1000 initS policyNet
  return ()
{-# INLINE reinforce #-}
