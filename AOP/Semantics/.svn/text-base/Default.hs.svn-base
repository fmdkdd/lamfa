{-# LANGUAGE TemplateHaskell,
             FlexibleInstances,
             UndecidableInstances,
             ScopedTypeVariables,
             MultiParamTypeClasses
 #-}
-- | Default aspect semantics, aspects are deployed and undeployed at the top level.
module AOP.Semantics.Default where
import AOP.Internal.AOPMonad
import AOP.Internal.Typeable1Monad
import AOP.Internal.Aspect
import AOP.Internal.AOTType
import AOP.Internal.Pointcut
import AOP.Internal.Function

-- | Default aspect semantics, aspects are deployed and undeployed at the top level.
instance (Typeable1Monad m, Typeable1Monad (t m)) => MonadDeploy t m where
      deployInEnv   asp aenv = return (EAspect asp:aenv)
      undeployInEnv asp@(Aspect (pc::PC (t m) a b) adv hnd) aenv =
                    return (deleteAsp (EAspect asp) aenv)