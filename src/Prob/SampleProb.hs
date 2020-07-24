{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module RL where


import Prelude hiding (zipWith)
import qualified GHC.Generics  -- To avoid coercion hole errors from ConCat.Plugin.

import GHC.Generics ((:*:)(..))

--import ConCat.GradientDescent
import ConCat.Deep
import ConCat.Misc
import ConCat.DUtils
import ConCat.Category
import qualified Data.Vector.Sized as VS
import           Data.Key     (Zip(..))

import ConCat.Misc     (R)
import ConCat.Rebox    ()  -- Necessary for reboxing rules to fire
import ConCat.AltCat   ()  -- Necessary, but I've forgotten why.
import ConCat.Classifiable
--import ConCat.Free.VectorSpace hiding (V)

-- Policy-Based RL

-- The Policy is parameterized and a probability distribution

--pi :: f g theta -> g s -> g a

type PType = ((V 5 --+ V 3))-- :*: (V 10 --+ V 5) :*: (V 4 --+ V 10))


--pi :: ((V 5 --+ V 3) :*: (V 10 --+ V 5) :*: (V 4 --+ V 10)) R


policy = affRelu @PType @PType @R


-- 2 components in diagonal gaussian policies:
-- 1)  u :: p -> s -> a    mean actions
-- 2) diagonal covariance matrix
     -- 1) single vector of log standard deviations which is not a function of the state, log sigma are standalone parameters
     --    logSig :: pSig -> stdev a
     -- 2) nn that maps from states to log standard deviations
     --    logSig :: pSig -> s -> stdev a
-- We Sample from such a policy by computing an action sample
-- a s = (u s) + (zipWith * (exp $ logSig s) (z))
   -- where
     -- z = sample normal 0 



class Policy p s a where
  sample :: p -> s -> a
  logProb :: p -> a -> R


action mu sig s z = (mu s) + (zipWith (*^) (sig s) z)

logLikelihood mu sigma k =  (- 1/2) * foldl (+) (\aI muI sigI -> ((aI - muI)^^2 / sigI^^2) + 2 log sigI) + (k * log 2 * pi)
