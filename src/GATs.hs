{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module GATs where

import Prelude hiding (zip, zipWith, id, (.), uncurry, curry)
import ConCat.Category
import ConCat.Deep
import ConCat.RAD
import ConCat.Additive
import ConCat.Nat

-- For Bush Trees
import ConCat.Shaped as T

import ConCat.AltCat (fork, Additive1)
import ConCat.Rebox ()
import GHC.Generics hiding (R)

import Data.Bifunctor

import Algebra.Graph.Labelled as G
import Algebra.Graph.ToGraph
import Utils hiding (R)
import ConCat.Misc
import Data.Key
import qualified Data.Set as S

{--
-- Attention based architecture to perform node classification of graph-structured data
-- Compute the hidden representations of each node in the graph by attending over its neighbors using self-attention

-- HasV s i, HasV s' o,

type AttnT r = (Additive r, Num r, Floating r, Fractional r)

type AttnCon p r = (Additive1 p, Zip p, Foldable p, Functor p, AttnT r)



newtype Neighborhood f e s = Neighborhood ((Eq e, Monoid e, Ord s, Functor f, Foldable f) => (G.Graph e s) -> s -> f (s :* s))

newtype Attn p q r a b = Attn ((q :*: p) r)



--instance (AttnCon p r, AttnCon q r) => Category (Attn p q r) where
--  id = inNew
  


attn :: forall p q r. (AttnCon p r, AttnCon q r) => p r -> q r -> p r
attn a b = sumA <$> (a >.< b)
{-# INLINE attn #-}

attnTree = attn @(LVec N5) @(LVec N5) @(R)

--instance  2 (Attn p q r s)  

--graphAttn :: forall p q p' s s' e r f g.
--  (AttnCon p r, AttnCon p' r, Eq e, Monoid e, Ord s, Functor f, Foldable f, Functor g, Foldable g)
--  => G.Graph e s
--  -> Neighborhood q e s
--  -> p r
--  -> p' r
--  -> f s
--  -> f s'
graphAttn = \graph nodeEmbed (Attn attn) (Neighborhood neighborFn) w a' nodeStates -> let
  nodeAttns =  softmax <$> (fmap compCoeffs) <$> ((neighborFn graph) <$> nodeStates)
  edgeAttns = undefined
  in (bimap graph nodeAttns edgeAttns)

attend :: (AttnCon p r, AttnCon q r, Bifunctor g) => p r -> q r -> G.Graph e n -> f r 
attend nodeEmbed edgeEmbed n e  = (uncurry attn) <$>
  (bimap nodeEmbed edgeEmbed)
{-# INLINE attend #-}

attnCoeffs :: (AttnCon p r) => (p r -> s -> s -> r) -> p r -> s -> s -> r
attnCoeffs = \attnP coeffP hi hj -> attnP coeffP hi hj 
{-# INLINE attnCoeffs #-}
--a = linear

attnMech :: (AttnCon p r) => p r -> q r -> r
attnMech = undefined

softmax :: (Functor f, Foldable f, Fractional a, Additive a, Floating a) => f a -> f a
softmax = normalize . (fmap (exp))
{-# INLINE softmax #-}

graphNeighbors :: (Eq e, Monoid e, Ord s) => Graph e s -> s -> [s :* s]
graphNeighbors graph n = (n,) <$> (S.toAscList $
                               (preSet n graph) `S.union` (postSet n graph))
{-# INLINE graphNeighbors #-}
--}
