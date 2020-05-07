{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module DDQN where

import Prelude hiding ((<>))
import GHC.Generics ((:*:))
import ConCat.Deep
import ConCat.Additive
import ConCat.AltCat ()
import ConCat.Rebox ()
import ConCat.AltCat (Additive1(..))
import Numeric.LinearAlgebra hiding (Additive, R)
import ConCat.Misc ((:*), R)

{--
data NetOpts = NetOpts
  { nEpochs :: Int
  , lr :: (Additive a, Num a) => a
  , network :: (Additive1 p, Zip p, Foldable b, Zip b, Additive s, Num s) => (p s -> a s -> b s)
  , networkParams :: (Additive1 p, Zip p, Additive s, Num s) => p s
  , trainingData :: (Functor f, Foldable f, Additive s, Num s) => f (a s :* b s)
  }
--}

{--
A VECTORSPACE IS A ZIPPABLE FUNCTOR


genTrainData :: Int -> [(Vector R :* Vector R)]
genTrainData nEx = xy
  where xy = map (\i-> (x i, y i)) [1..nEx]
        x = (\i->vector [1..(fromIntegral 100)])
        y = (\i-> vector [1])

getNetworkParams :: Int -> (Int, Int) -> Int -> [Matrix R]
getNetworkParams nLayers wDim nOut = ls ++ [outL]
  where
    ls = map (\i -> layer wDim) [1..nLayers]
    outL = layer (fst wDim, nOut)

    layer wD = weight wD -- , bias (snd wD))
    -- bias r = vector [1..(fromIntegral r)]
    weight w = matrix rs [1..numEls]
      where
        numEls = foldl (*) 0 (map fromIntegral [rs, cs])
        rs = fst w
        cs = snd w


ffn :: [Vector R] -> (Matrix R, Vector R) -> [Vector R]
ffn exs (w, b) =  map (\x -> w #> x + b) exs 

network :: [(Matrix R, Vector R)] -> [Vector R] -> [Vector R]
network params xs = (foldl ffn xs params)

run = trainNTimes nEpoch lr lr3 netParams xys
  where netParams = getNetworkParams 4 (100, 100) 10
        nEpoch = 10
        lr = 0.003
        xys = genTrainData 2000
--}
-- putStrLn "Trained Several Times!"

{--
class HasLayers p where
  getWeights :: p s -> [[s]]
  getBiases  :: p s -> [[s]]

instance (HasLayers f, HasLayers g) => HasLayers (g :*: f) where
  getWeights (g :*: f) = [ (concat . getWeights) f, (concat . getWeights) g ]
  getBiases  (g :*: f) = [ (concat . getBiases)  f, (concat . getBiases)  g ]

instance (Foldable a, Foldable b) => HasLayers (a --+ b) where
  getWeights (Comp1 gf) = (map (toList          . fstF) . toList) gf
  getBiases  (Comp1 gf) = (map ((: []) . unPar1 . sndF) . toList) gf


data (Num s, Additive s) => Layer s = Layer s
  { weight :: Matrix s
  , bias   :: Vector s
  } deriving (Functor, Eq, Ord, Show)


instance HasLayers Layer where
  getWeights Layer{weight} = toList . rows weight
  getBiases Layer{bias} = toList . rows bias

instance Additive1 Layer where
  

instance Zip Layer where
  
--}
