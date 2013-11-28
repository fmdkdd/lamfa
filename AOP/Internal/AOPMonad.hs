{-# LANGUAGE MultiParamTypeClasses,
             FlexibleContexts
 #-}

module AOP.Internal.AOPMonad (
AOPMonad(..),
MonadDeploy(..),
) where

import AOP.Internal.Aspect
import AOP.Internal.LessGen
import AOP.Internal.Typeable1Monad
import AOP.Internal.OpenApp
import AOP.Internal.Function

-- A monad that does top-level deploy/undeploy
class (Typeable1Monad m, OpenApp (->) m, OpenApp Function m) => AOPMonad m where
      deploy   :: LessGen (a -> b) (c -> m d) => Aspect m a b c d -> m ()
      undeploy :: LessGen (a -> b) (c -> m d) => Aspect m a b c d -> m ()


-- A MonadDeploy monad defines how to deploy and undeploy aspects
class (Typeable1Monad m, Typeable1Monad (t m)) => MonadDeploy t m where
      deployInEnv   :: LessGen (a -> b) (c -> t m d) => 
                       Aspect (t m) a b c d -> 
                       AspectEnv (t m) -> m (AspectEnv (t m))
      undeployInEnv :: LessGen (a -> b) (c -> t m d) => 
                       Aspect (t m) a b c d -> 
                       AspectEnv (t m) -> m (AspectEnv (t m))
