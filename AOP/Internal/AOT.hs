{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts
 #-}

module AOP.Internal.AOT (
 AOT,
 runAOT,
 wappt,
) where

import AOP.Internal.Typeable1Monad
import AOP.Internal.Aspect
import AOP.Internal.Pointcut
import AOP.Internal.Advice
import AOP.Internal.Joinpoint
import AOP.Internal.AOPMonad
import AOP.Internal.OpenApp
import AOP.Internal.Function
import AOP.Internal.AOTType

import Unsafe.Coerce

-- | Runs an AOT computation to obtain a computation in the underlying monad
runAOT :: Typeable1Monad m => AOT m a -> m a
runAOT c = liftM fst $ run c []

-- | Monadic weaver
weavet :: (Typeable1Monad m) => (a -> AOT m b) -> AspectEnv (AOT m) -> AspectEnv (AOT m) ->
                                Jp (AOT m) a b -> m ((a ->  AOT m b) , AspectEnv (AOT m))
weavet f [] fenv _ = return (f,fenv)
weavet f (asp:asps) fenv jp = 
 case asp of EAspect (Aspect pc adv _) -> do (match, fenv') <- run (runPC pc jp) fenv
                                             weavet (if match
                                                        then apply_adv adv f
                                                        else f)
                                                     asps fenv' jp

-- | Implementation of woven application for AOT, used in the overloading of #.
-- FunctionTag argument is used for function identity:
--     Wrapped functions are equal when their tags are equal
--     Regular functions share the same tag, and are compared using StableNames.
wappt :: (Typeable1Monad m, PolyTypeable (a -> AOT m b)) => (a -> AOT m b) -> FunctionTag -> a -> AOT m b
wappt f t a = mkAOT $ \ aenv ->
                  do (woven_f, fenv) <- weavet f aenv aenv (newjp f t a)
                     run (woven_f a) fenv

-- | Every regular functions is tagged with the same default tag.
instance Typeable1Monad m => OpenApp (->) (AOT m) where
         f # a = wappt f defaultFunctionTag a

-- | Function is a wrapper to add a notion of identity based on tags.
instance Typeable1Monad m => OpenApp Function (AOT m) where
         (Function f t) # a = wappt f t a

-- | Typeable instance so types of computations in AOT can be compared (like in pcCall and pcType)
instance Typeable1Monad m => Typeable1 (AOT m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "AOT" "AOT" "AOT") [typeOf1 (undefined :: m ())]

-- | The semantics of aspect deployment are defined in the
-- MonadDeploy typeclass. AOT assumes it is on top of an MonadDeploy
-- instance, and uses that functions for aspect deployment.
instance (Typeable1Monad m, MonadDeploy AOT m) => AOPMonad (AOT m) where
      deploy asp   = mkAOT $ \aenv ->
                         do aenv' <- deployInEnv asp aenv
                            return ((), aenv')
      undeploy asp = mkAOT $ \aenv ->
                         do aenv' <- undeployInEnv asp aenv
                            return ((), deleteAsp (EAspect asp) aenv')


-- Interaction with standard monad transformers not derived automatically

data RPair s a = RPair a s
instance Functor (RPair s) where
  fmap f (RPair a s) = RPair (f a) s

toRP   (a,s)       = RPair a s
fromRP (RPair a s) = (a,s)

instance MonadTrans AOT where
         lift ma = mkAOT $ \aenv -> do
                       a <- ma
                       return (a, aenv)
         -- mt       = MT
         -- unlift f = mkAOT $ \aenv ->
         --            f (unsafeCoerce (\m -> run m aenv >>= return . toRP)) >>= return . fromRP 

instance MonadState s m => MonadState s (AOT m) where
         get = lift get
         put = lift . put

instance MonadError s m => MonadError s (AOT m) where
         throwError = lift . throwError
         ma `catchError` h = mkAOT $ \aenv ->
            run ma aenv `catchError` \e -> run (h e) aenv

instance MonadWriter w m => MonadWriter w (AOT m) where
    tell     = lift . tell
    listen m = mkAOT $ \aenv -> do
               ((a, aenv'), w) <- listen (run m aenv)
               return ((a, w), aenv')
    pass m = mkAOT $ \aenv -> pass $ do
             ((a, f), aenv') <- run m aenv
             return ((a, aenv'), f)

instance (MonadReader r m) => MonadReader r (AOT m) where
    ask       = lift ask
    local f m = mkAOT $ \s -> local f (run m s)

