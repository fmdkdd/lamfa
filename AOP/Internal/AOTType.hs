{-# LANGUAGE RankNTypes,
             ExistentialQuantification,
             ImpredicativeTypes,
             TypeOperators,
             GeneralizedNewtypeDeriving
 #-}

module AOP.Internal.AOTType (
  run,
  mkAOT,
  AOT (),
  run_s,
  mkAOT_s,
  AOT_s (),
  run_sp,
  mkAOT_sp,
  AOT_sp (),
  -- AOTZ (..),
  -- AOT_zipper (..),
  EPC (..),
  SP (..),
) where

import AOP.Internal.Aspect
import AOP.Internal.Pointcut
-- import Control.Monad.Zipper
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Cont

-- | This module defines the AOT type. It is a separate module to avoid circular dependencies between modules.

data EPC m = forall a b. EPC (PC m a b)
data SP m = forall a1 b1. SP (PC m a1 b1) (AspectEnv m)

newtype AOT m a = AOT { unAOT :: StateT (AspectEnv (AOT m)) m a }
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)
run = runStateT . unAOT
mkAOT = AOT . StateT


newtype AOT_s m a = AOT_s { unAOT_s :: StateT (AspectEnv (AOT_s m), EPC (AOT_s m)) m a }
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)
run_s = runStateT . unAOT_s
mkAOT_s = AOT_s . StateT

newtype AOT_sp m a = AOT_sp { unAOT_sp :: StateT (AspectEnv (AOT_sp m), SP (AOT_sp m)) m a }
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)
run_sp = runStateT . unAOT_sp
mkAOT_sp = AOT_sp . StateT

-- type AOTZ = (IdentityT :> AOT_zipper)
-- newtype AOT_zipper m a = AOT_zipper { run_zipper :: AspectEnv (AOTZ m) -> m (a, AspectEnv (AOTZ m)) }
