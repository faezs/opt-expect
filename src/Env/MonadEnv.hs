{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Env.MonadEnv (MonadEnv(..), sampleIOE) where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Generics


newtype MonadEnv a = MonadEnv { runMonadEnv :: SamplerIO a } deriving (Functor, Applicative, Monad, MonadIO, Generic)

instance MonadThrow MonadEnv where
  throwM = MonadEnv . liftIO . throwM


instance MonadBase IO MonadEnv where
  liftBase = MonadEnv . liftIO

instance MonadBaseControl IO MonadEnv where
  type StM MonadEnv a = a
  -- liftBaseWith :: ((forall a. m a -> IO (StM m a)) -> IO a) -> m a
  liftBaseWith f = liftIO $ liftBaseWith @IO @IO (\run -> f (run . sampleIO . runMonadEnv)) 
  restoreM a = liftIO $ restoreM @IO @IO a

deriving instance MonadSample MonadEnv

sampleIOE :: MonadEnv a -> IO a
sampleIOE = sampleIO . runMonadEnv
