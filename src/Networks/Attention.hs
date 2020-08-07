{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Networks.Attention where

import Prelude hiding ((.), id, curry, uncurry, zip)
import ConCat.Misc
import ConCat.Deep
import ConCat.Additive
import GHC.Generics
import qualified ConCat.TArr as T
import ConCat.Category
--import ConCat.Free.LinearRow as L

-- use a fixed point functor h
-- with a duplicative structure on h 
--type Multihead h e = h e -> e

--multihead es = norm . add

import Data.Functor.Adjunction
import Data.Functor.Rep
import Data.Key


class HasP i pw o where
  params :: CospanL i pw o
  fold :: i -> pw -> o 
  


newtype CospanL i p o = CospanL (((i -> p -> o) :* p) -> i) deriving (Generic)

class HasDim sh a where
  dim :: a -> sh

type MultiHead p i o = CospanL [i] ((i -> p -> o) -> ([o] -> o)) o
type FFN p s = CospanL s (p -> s -> s) s
type AddNorm p s i o = CospanL i (p -> i -> o) o

type Attendable f a = (Representable f, T.HasFin' a)


--attention a b = zeroC
--attention b () = affRelu b
{--
attention :: (Foldable a, Foldable a1, Zip a,
                                 Zip a1, Ord s, Ord s1, Additive s,
                                 Additive s1, Num s, Num s1, Functor b,
                                 Functor b1) =>
                                (--+) a b s
                                -> (--+) a1 b1 s1
                                -> (a s) :+ (a1 s1)
                                -> (b s) :+ (b1 s1)
--}

--attention = Attention(Q,K,V)=softmax(QK⊤n−−√)V


type A a1 a2 b a3 c = ((--+) a1 ((->) a2) b -> (:*:) a3 a1 b -> a2 -> c)

type V a = (Foldable a, Zip a)
type Sc s = (Additive s, Num s, Floating s, Ord s)

-- Attention forms a category via affine map.
-- A Bumped Generalized Matrix is a morphism from one Representable Functor to another.
-- The pipeline is such : let 
attention :: forall k q d v a p. (V q, V k, V v, Sc a, V p, V k, V d) => (k --+ d) a -> (q --+ d) a -> (v --+ d) a -> k a -> q a -> v a -> d a
attention qw kw vw q k v =  dotV v' <$> (keyed_question k' q')  --dotV (affLog (q  k)) (affRelu v)
  where
    k' = linear kw (bump k)
    q' = linear qw (bump q)
    v' = linear vw (bump v)
    --keyed_question :: f a -> g a -> f (g a)
    keyed_question :: (V f, V g, Sc a, Ord a) => f a -> g a -> g (f a)
    keyed_question q k = softmax (mask . (scale sf) $ (matmul q k))
      where
        matmul :: (V f, V g, Sc a) => f a -> g a -> g (f a)
        matmul fs gs = gs >.< fs
        scale :: forall f g a. (V f, V g) => (Functor f, Functor g, Num a) => a -> g (f a) -> g (f a)
        scale a ba = (scaleV a) <$> ba
        mask ::  (Functor f, Functor g) => g (f a) -> g (f a)
        mask ba = dropout <$> ba
          where dropout = undefined
        softmax :: (Functor g, Functor f, Foldable f, Additive a, Num a, Ord a) => g (f a) -> g (f a)
        softmax xs = normalize <$> ((fmap.fmap $ exp) xs) 
        sf :: a
        sf = undefined




attend :: forall f g a b. (Attendable f a, Attendable g b) => f a -> f b 
attend = undefined

--type Attn h aw fw = (HasDim fw h, HasDim aw h) => (AddNorm (a))

--attn :: forall i o heads fw ff2 lin. Embed [i] -> PosEncoding (Embed i) ->  Attn h w


--positionalEncoding :: Embedding w ->


-- All the model sublayers and the embedding layer
-- have outputs of modelDim 512.

--data Layer d f s = Layer (f d, s d)

data Encoder p d = Stack [Encoder p d]
  | Layer (Encoder p d) (Encoder p d) -- A layer is made up of a self attention layer and a 
  | SelfAttention (MultiHead d p d)
  | Norm (AddNorm p (Encoder p d, Encoder p d) (Encoder p d) (Encoder p d))
  | FF p d
  deriving (Generic)
