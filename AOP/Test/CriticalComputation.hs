{-# LANGUAGE TemplateHaskell,
             ScopedTypeVariables,
             FlexibleContexts,
             MultiParamTypeClasses,
             GeneralizedNewtypeDeriving,
             DeriveDataTypeable
  #-}

module AOP.Test.CriticalComputation (
stdComputation,
criticalComputation,
runSafe
) where

import AOP.Default
import AOP.JPStackT
import AOP.Cflow
import AOP.MonadJPStack

-- The new AOT_cflow transformer is easily obtained using newtype and deriving clauses.
-- This way it is simple to create new computational stacks with security policies.
newtype AOT_cflow m a = AOT_cflow (AOT_s (JPStackT m) a)
        deriving (Monad, OpenApp (->), OpenApp Function, AOPMonad, MonadJPStack) 

instance Typeable1Monad m => Typeable1 (AOT_cflow m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "AOT_s'" "AOT_s'" "AOT_s'") [typeOf1 (undefined :: m ())]

-- Lifts an AOT_s function into an AOT_cflow one
clift :: Typeable1Monad m => Function a (AOT_s (JPStackT m) b) -> Function a (AOT_cflow m b)
clift (Function f t) = mkFunction (\x -> AOT_cflow (f # x)) t


-- Runs computation c, protecting join points in the cflow of _criticalComputation from weaving
runSafe (AOT_cflow c) = evalJPStackT (runAOT_s (EPC (pcCflow _criticalComputation)) c) []

stdTag = $newTag
stdComputation :: Monad m => Function Integer (m Integer)
stdComputation = mkFunction (\n -> return (n+32)) stdTag

criticalTag = $newTag
_criticalComputation :: Typeable1Monad m => Function Integer (AOT_s m Integer)
_criticalComputation  = mkFunction (\n -> stdComputation # (n+1)) criticalTag

criticalComputation :: Typeable1Monad m => Function Integer (AOT_cflow m Integer)
criticalComputation = clift _criticalComputation