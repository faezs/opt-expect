{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module GATs where

import Prelude hiding (zip, zipWith, id, (.), uncurry, curry)
import ConCat.Category
import ConCat.Deep
import ConCat.RAD
import ConCat.Additive

import ConCat.AltCat (fork, Additive1)
import ConCat.Rebox ()
import GHC.Generics hiding (R)

import Algebra.Graph.Labelled as G
import Algebra.Graph.ToGraph
import Utils hiding (R)
import ConCat.Misc
import Data.Key
import qualified Data.Set as S

-- Attention based architecture to perform node classification of graph-structured data
-- Compute the hidden representations of each node in the graph by attending over its neighbors using self-attention

-- HasV s i, HasV s' o,

type AttnCon p r = (Additive1 p, Zip p, Additive r, Num r, Floating r, Fractional r)

newtype Neighborhood f e s = Neighborhood ((Eq e, Monoid e, Ord s, Functor f, Foldable f) => G.Graph e s -> s -> f (s, s))

newtype PairwiseAttn p s r = PairwiseAttn ((AttnCon p r) => (p r -> s -> s -> r))

graphAttn :: forall p p' s s' e r f g.
  (AttnCon p r, AttnCon p' r, Eq e, Monoid e, Ord s, Functor f, Foldable f, Functor g, Foldable g)
  => G.Graph e s
  -> (p r -> s -> s')
  -> PairwiseAttn p' s' r
  -> Neighborhood g e s
  -> p r
  -> p' r
  -> f s
  -> f s'
graphAttn graph = \nodeEmbed (PairwiseAttn mechanism) (Neighborhood neighborFn) w a' nodeStates -> let
  compCoeffs :: (s, s) -> r 
  compCoeffs = (\(n, n') -> attnCoeffs mechanism a' n n') . (nodeEmbed w *** nodeEmbed w)
  nodeAttns =  softmax <$> (fmap compCoeffs) <$> ((neighborFn graph) <$> nodeStates)
  
  in undefined


attnCoeffs :: (AttnCon p r) => (p r -> s -> s -> r) -> p r -> s -> s -> r
attnCoeffs = \attnP coeffP hi hj -> attnP coeffP hi hj 
{-# INLINE attnCoeffs #-}
--a = linear

attnMech :: (AttnCon p r) => PairwiseAttn p s r
attnMech = undefined

softmax :: (Functor f, Foldable f, Fractional a, Additive a, Floating a) => f a -> f a
softmax = normalize . (fmap (exp))
{-# INLINE softmax #-}

graphNeighbors :: (Eq e, Monoid e, Ord s) => Graph e s -> s -> [s :* s]
graphNeighbors graph n = (n,) <$> (S.toAscList $
                               (preSet n graph) `S.union` (postSet n graph))
{-# INLINE graphNeighbors #-}
