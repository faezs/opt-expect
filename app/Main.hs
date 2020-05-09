{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

module Main where

import GHC.Generics (Par1(..),(:*:)(..),(:.:)(..))
import ConCat.AltCat  (Additive1(..),(<+))
import ConCat.Rebox ()

import CartPole
import PPO
import Utils

main :: IO ()
main = reinforce


reinforce :: IO ()
reinforce = do
  initS <- initCPIO
  let
    policyNet = (gaussInit <$> randF 1 :: PType 4 16 2)
    valueNet = (gaussInit <$> randF 1 :: PType 4 16 1)
  mapM_ (putStrLn . show) $ runCPEpisode initS $ agent $ last $ runEps runCPEpisode 100 initS policyNet
  return ()
{-# INLINE reinforce #-}
