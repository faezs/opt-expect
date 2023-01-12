{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}
--{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}
module RL.PPO where

import Prelude hiding (zip, zipWith, length)
import Data.Foldable
import ConCat.Deep
import ConCat.Misc
import ConCat.Rebox  -- Necessary for reboxing rules to fire
import ConCat.AltCat (forkF, fork, fromIntegralC, sumAC, Additive1, (<+), additive1)  -- Necessary, but I've forgotten why.
import ConCat.RAD (gradR)
import ConCat.Additive ((^+^), Additive, sumA)
import ConCat.Sized
import ConCat.GradientDescent (maximize)


import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import Data.Key
import qualified Data.Vector.Generic as VG

import GHC.Generics hiding (R)
import GHC.TypeLits
import Data.Proxy
import Data.Finite

import Utils.Utils hiding (R)
import Control.Monad.IO.Class
import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Trans.Reader
import Prob.Policy
import Env.Env
import Optimizers.Adam

import Streamly.Prelude ( adapt, MonadAsync, IsStream )
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

type (f ->~ g) s = (Functor f, Functor g, Foldable g, Fractional s, Floating s, Additive s)

softmax :: ((f  ->~ g) s) => Unop (f (g s))
softmax = fmap (normalize . fmap exp)
{-# INLINE softmax #-}

valueFn :: (KnownNat i, KnownNat h) => PType i h 1 -> (V i --> V 1) R
valueFn = lr3'
{-# INLINE valueFn #-}

policy :: (KnownNat i, KnownNat h, KnownNat o) => PType i h o -> (V i --> V o) R
policy = lr3'  -- Defined in ConCat.Deep.
{-# INLINE policy #-}


-- Any enumerable is a categorical distribution
catAgent :: forall m i h o s a. (MonadDistribution m, HasV s i, HasV a o, Enum a, KnownNat h) => PType i h o -> s -> m a
catAgent ps s = toEnum <$> (sampleCat . (softmax . policy $ ps) . toV) s
{-# INLINE catAgent #-}

forkV :: forall i o o'. (KnownNat i, KnownNat o, KnownNat o') => ((V i --> V o) R, (V i --> V o') R) -> (V i R -> (V o R, V o' R))
forkV = fork
{-# INLINE forkV #-}


-- a mu and an std represent a distribution
gaussAgent :: forall m i h o s a. (MonadDistribution m, HasV s i, HasV a o, KnownNat h) => PType i h o -> PType i h o -> s -> m a
gaussAgent mu std s = fromV <$> uncurry sampleGaussian (forkV (policy mu, policy std) $ toV s)
{-# INLINE gaussAgent #-}


{----------------------------------- PPO-Clip ------------------------------------------}


type RLCon s a i h o =  (KnownNat i, KnownNat h, KnownNat o, HasV s i, HasV a o)
type KnownNat3 a b c = (KnownNat a, KnownNat b, KnownNat c)

valueFnLearn :: forall n s a i h. (HasV s i, KnownNat h, KnownNat n) => R -> V n (Transition s a R) -> PType i h 1 -> PType i h 1
valueFnLearn = \gamma eps p -> steps valueFn gamma (t2io eps) p
  where
    t2io :: V n (Transition s a R) -> V n (V i R, V 1 R)
    t2io = fmap (\Transition{..} -> (toV s_t, VS.singleton rw))
{-# INLINE valueFnLearn #-}

valueFold :: forall n m s a r i h o.
            RLFold n m s a r i h o
         => NNFold n m s a r i h 1
valueFold lr eta v = FL.foldlM' (flip step) begin
  where
    begin :: m (PType i h 1)
    begin = pure v
    step :: V n (Transition s a R) -> PType i h 1 -> m (PType i h 1)
    step v = pure . valueFnLearn lr v
    end :: PType i h 1 -> m (PType i h 1)
    end = pure
{-# INLINE valueFold #-}

policyLearn :: forall n s a i h o. (RLCon s a i h o, KnownNat n) => (PType i h o -> V i R -> V o R) -> R -> V n (Transition s a R) -> PType i h o -> PType i h o
policyLearn = \policyFn gamma eps p -> steps policyFn gamma (t2io eps) p
  where
    t2io :: V n (Transition s a R) -> V n (V i R, V o R)
    t2io = fmap (\Transition{..} -> (toV s_t, toV a_t))
{-# INLINE policyLearn #-}

type RLFold n m s a r i h o = (RLCon s a i h o, KnownNat n, MonadAsync m)

policyFn' :: (PType i h o -> V i R -> V o R)
policyFn' = undefined

type LR = R
type Eta = R


scanBatch  :: forall n t m i h o s a r.
  (RLCon s a i h o, KnownNat n, MonadAsync m, IsStream t)
  => NNFold n m s a r i h o
  -> LR
  -> Eta
  -> PType i h o
  -> t m (Transition s a R)
  -> t m (PType i h o)
scanBatch fl lr eta pi = S.postscan (fl lr eta pi) . minibatch @n
         -- $ S.trace (liftIO . print . ("PType Evolution" <>) . show . sumA . uncurry (^-^))
{-# INLINE scanBatch #-}



pvFold :: forall n m s a r i h o.
            RLFold n m s a r i h o
        => LR -> Eta -> (PType i h 1, PType i h o) -> FL.Fold m (V n (Transition s a R)) (PType i h 1, PType i h o)
pvFold lr eta (v, p) = FL.tee (valueFold @n @m @s @a @r @i @h @o lr eta v) (ppoFold lr eta p)
{-# INLINE pvFold #-}

type NNFold n m s a r i h o = (LR -> Eta -> PType i h o -> FL.Fold m (V n (Transition s a R)) (PType i h o))


ppoFold :: forall n m s a r i h o.
            RLFold n m s a r i h o
        => NNFold n m s a r i h o
ppoFold lr eta pi = FL.rmapM (pure . snd) $ FL.foldlM' (flip step) begin
  where
    begin :: m (PType i h o, PType i h o)
    begin = pure (pi, pi)
    step :: V n (Transition s a R) -> (PType i h o :* PType i h o) -> m (PType i h o :* PType i h o)
    step txs pis@(piOld, piNew) = do
      let del = ppoGrad eta txs pis
      -- liftIO . print $ "Grad: " <> (show del)
      -- liftIO . print $ "piOld: " <> (show piOld)
      -- liftIO . print $ "piOld: " <> (show piNew)
      return (piNew, piNew ^+^ (lr *^ del))
    end :: (PType i h o, PType i h o) -> m (PType i h o, PType i h o)
    end = pure
{-# INLINE ppoFold #-}


ppoGrad :: forall n i h o s a. (RLCon s a i h o, KnownNat n) => Eta-> V n (Transition s a R) -> (PType i h o, PType i h o) -> PType i h o
ppoGrad eta trajectories (piOld, pi) = expect $ (\ Transition{..} ->
                                                     gradR (\p ->
                                                               (ppoLoss eta (toV s_tn) (toV a_t) rw (p, piOld))) pi)
                                           <$> trajectories
{-# INLINE ppoGrad #-}


attn :: (Foldable w, Additive1 w, Zip w, Num a, Fractional a, Additive a) => w a -> a
attn scores = sumA $ zipWith (*) scores (rep (recip $ sumA scores))
  where
    rep :: a -> w a
    rep = undefined

-- ppoGrad' :: forall n i h o s a. (RLCon s a i h o, KnownNat n) => Eta -> V n (Transition s a R) -> (PType i h o, PType i h o) -> PType i h o
-- ppoGrad' = \ eta tx pis@(piOld, pi) -> expect $ fmap (\t -> gradR (\ p -> ppoLoss eta t (p, piOld)) pi) tx
-- {-# INLINE ppoGrad' #-}

-- ppoGrad :: forall i h o s a. (RLCon s a i h o)
--         => Eta
--         -> Transition s a R
--         -> Unop (PType i h o, PType i h o)
-- ppoGrad = \ e t -> gradR (ppoLoss e t)
-- {-# INLINE ppoGrad #-}


ppoLoss :: forall i h o s a. (KnownNat3 i h o) => Eta -> V i R -> V o R -> R -> (PType i h o :* PType i h o) -> R
ppoLoss eta s a advantage p = policyRatio s a p `min` g eta advantage
{-# INLINE ppoLoss #-}

policyRatio :: (KnownNat3 i h o) => V i R -> V o R -> (PType i h o :* PType i h o) -> R
policyRatio s a (pi', pi) = logProb pi s a / logProb pi' s a
{-# INLINE policyRatio #-}


g :: R -> R -> R
g eta adv = if (adv >= 0) then (1 + eta) * adv else (1 - eta) * adv
-- g eta adv
--   | adv >= 0 = (1 + eta) * adv
--   | otherwise = (1 - eta) * adv
{-# INLINE g #-}



{-------------------------------- Vanilla Policy Gradient -----------------------------}


policyGradient :: forall n i h o s a p. (KnownNat n, HasV s i, HasV a o, KnownNat h) => LR -> V n (Transition s a R) -> (PType i h o -> s -> a) ->  Unop (PType i h o)
policyGradient lr tx pol p = p ^+^ (lr *^ gradLogProbExp tx p)
{-# INLINE policyGradient #-}

type ParamCon p = (Functor p, Zip p, Additive1 p)

-- Expectation over the grad log prob kf the advantage-normalized for all trajectories of an episode
gradLogProbExp :: forall n i h o s a. (KnownNat n, HasV s i, HasV a o, KnownNat h) => V n (Transition s a R) -> Unop (PType i h o)
gradLogProbExp tx p = expect (fmap (\Transition{..} -> gradLogProb advantage (toV s_t) (toV a_t) p) tx)
{-# INLINE gradLogProbExp #-}

type Adv = R

-- The log probablitity is multiplied by the log prob of the action given the state to avoid threading it though
-- into the expectation, which is just a sum 
gradLogProb :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => Adv -> V i R -> V o R -> PType i h o -> PType i h o
gradLogProb genAdv st act = gradR (\ps -> genAdv * logProb ps st act) -- (genAdv * (logProb params st act)) *^ params --
{-# INLINE gradLogProb #-}

logProb :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => PType i h o -> V i R -> V o R -> R
logProb = log . prob
{-# INLINE logProb#-}

-- $ probability over the actions is the inner product of the policy with the actions
prob :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => PType i h o -> V i R -> V o R -> R
prob ps st act = policy @i @h @o ps st <.> act
{-# INLINE prob#-}

-- attn :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => PType i h o -> V i R -> V o R -> (V i --> V o) R
-- attn ps st act = policy @i @h @o ps st >.< act

-- Mean over an additive functor
expect :: (Zip f, Functor f, Foldable f, Additive a, Num a, Functor g, Additive (g a), Fractional a) => f (g a) -> g a
expect fs = sumA fs ^/ (fromIntegral . length $ fs)
{-# INLINE expect #-}
