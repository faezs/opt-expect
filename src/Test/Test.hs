{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test where

import ConCat.Deep
--import ConCat.Misc
import Utils
import qualified Numeric.LinearAlgebra.Static as H
import qualified Data.Vector.Sized as VS
import ConCat.RAD (gradR)
import MLUtils
import GHC.Generics hiding (R)
import Data.Default

import ConCat.Rebox ()
import ConCat.AltCat (toCcc)
import ConCat.Shaped
import Env
import Streamly
import qualified Streamly.Prelude as S
import CircMisc

--import PPO


a :: PType 10 32 10 -> V 10 Double -> V 10 Double
a = lr3'
{-# INLINE a #-}

ps = (pure 1 :: PType 10 32 10)
{-# INLINE ps #-}
--runA = (a ) <$> )

xs = (replicate 100 (pure 1 :: V 10 Double))
{-# INLINE xs #-}

ys = (replicate 100 (pure 1 :: V 10 Double))
{-# INLINE ys #-}

a' = steps a 0.3 (zip xs ys) ps
{-# INLINE a' #-}


type RTx = Transition R R R 

txs :: SerialT MonadEnv RTx
txs = S.replicate 100 (def)

g :: PType 10 32 10 -> R
g = const 10
{-# INLINE g #-}

t = gradR g ps
{-# INLINE t #-}

--a'' = (policyGradient) 

b :: H.L 100 100 -> H.L 100 100
b i = i H.<> net H.<> net H.<> net 
  where
    net :: H.L 100 100
    net = H.matrix (replicate (100*100) 1)

runB = b ((H.matrix $ replicate (100*100) 1) :: H.L 100 100)

c = runCirc  "linear" $ toCcc $ (linear @(V 10) @(V 10) @R)
{-# INLINE c #-}
--c' = runSynCircDers "ppoLoss"
  --print $ foldr (H.<.>) runB
