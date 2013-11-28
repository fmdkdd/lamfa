{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             RankNTypes
 #-}

module AOP.Internal.AOT_secure (
 AOT_s,
 runAOT_s,
 wappt_s,
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

-- | Runs an AOT_s computation to obtain a computation in the underlying monad
runAOT_s :: Monad m => EPC (AOT_s m) -> AOT_s m a -> m a
runAOT_s pc c = liftM fst $ run_s c ([], pc)

-- | Monadic weaver
weavet_s :: Typeable1Monad m => 
            (a -> AOT_s m b) -> AspectEnv (AOT_s m) -> AspectEnv (AOT_s m) ->
            EPC (AOT_s m) -> Jp (AOT_s m) a b -> m (a -> AOT_s m b, AspectEnv (AOT_s m))
weavet_s f aenv fenv pcG@(EPC _pcG) jp = do
         (match, (fenv', pcG')) <- run_s (runPC _pcG jp) (fenv, pcG)
         if match
            then return (f, fenv)
            else weavet_saux f aenv fenv' pcG' jp

weavet_saux f [] fenv pcG _ = return (f, fenv)
weavet_saux f aenv@(asp:asps) fenv pcG jp = 
 case asp of
 EAspect (Aspect pc adv _) ->
  do  (match, (fenv', pcG')) <- run_s (runPC pc jp) (fenv, pcG)
      weavet_s (if match  
                   then (apply_adv adv f)
                   else f) asps fenv' pcG' jp

-- | Implementation of woven application for AOT_s, used in the overloading of #.
-- FunctionTag argument is used for function identity:
--     Wrapped functions are equal when their tags are equal
--     Regular functions share the same tag, and are compared using StableNames.
wappt_s :: (Typeable1Monad m, PolyTypeable (a -> AOT_s m b)) => 
           (a -> AOT_s m b) -> FunctionTag -> a -> AOT_s m b
wappt_s f t a = mkAOT_s $ \ (aenv, pc) -> do
                (woven_f, fenv) <- weavet_s f aenv aenv pc (newjp f t a)
                run_s (woven_f a) (fenv, pc)

-- | Every regular functions is tagged with the same default tag.
instance Typeable1Monad m => OpenApp (->) (AOT_s m) where
         f # a = wappt_s f defaultFunctionTag a

-- | Function is a wrapper to add a notion of identity based on tags.
instance Typeable1Monad m => OpenApp Function (AOT_s m) where
         (Function f t) # a = wappt_s f t a

-- | Typeable instance so types of computations in AOT_s can be compared (like in pcCall and pcType)
instance Typeable1Monad m => Typeable1 (AOT_s m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "AOT_s" "AOT_s" "AOT_s") [typeOf1 (undefined :: m ())]

-- | The semantics of aspect deployment are defined in the
-- MonadDeploy typeclass. AOT_s assumes it is on top of an MonadDeploy
-- instance, and uses that functions for aspect deployment.
instance (Typeable1Monad m, MonadDeploy AOT_s m) => AOPMonad (AOT_s m) where
      deploy asp   = mkAOT_s $ \ (aenv, pc) -> do
                     aenv' <- deployInEnv asp aenv
                     return ((), (aenv',pc))
      undeploy asp = mkAOT_s $ \ (aenv, pc) -> do
                     aenv' <- undeployInEnv asp aenv
                     return ((), (deleteAsp (EAspect asp) aenv, pc))


-- Interaction with standard monad transformers

data RPair s a = RPair a s
instance Functor (RPair s) where
  fmap f (RPair a s) = RPair (f a) s

toRP   (a,s)       = RPair a s
fromRP (RPair a s) = (a,s)

instance MonadTrans AOT_s where
         lift ma = mkAOT_s $ \ (aenv, pc) -> do
                   a <- ma
                   return (a, (aenv, pc))
         mt = MT
         unlift f = mkAOT_s $ \ (aenv, pc) ->
                    f (unsafeCoerce (\m -> run_s m (aenv, pc) >>= return . toRP)) >>= return . fromRP 

instance MonadState s m => MonadState s (AOT_s m) where
         get = lift get
         put = lift . put

instance MonadError s m => MonadError s (AOT_s m) where
         throwError = lift . throwError
         ma `catchError` h = mkAOT_s $ \ (aenv, pc) ->
            run_s ma (aenv, pc) `catchError` \e -> run_s (h e) (aenv, pc)

instance MonadWriter w m => MonadWriter w (AOT_s m) where
    tell     = lift . tell
    listen m = mkAOT_s $ \ (aenv, pc) -> do
               ((a, ctx), w) <- listen (run_s m (aenv, pc))
               return ((a, w), ctx)
    pass m   = mkAOT_s $ \ (aenv, pc) -> pass $ do
               ((a, f), ctx) <- run_s m (aenv, pc)
               return ((a, ctx), f)

instance (MonadReader r m) => MonadReader r (AOT_s m) where
    ask       = lift ask
    local f m = mkAOT_s $ \ (aenv, pc) -> local f (run_s m (aenv, pc))
