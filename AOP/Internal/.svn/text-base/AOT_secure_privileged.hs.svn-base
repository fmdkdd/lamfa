{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts
 #-}

module AOP.Internal.AOT_secure_privileged (
 AOT_sp,
 runAOT_sp,
 wappt_sp,
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

-- | Runs an AOT_sp computation to obtain a computation in the underlying monad
runAOT_sp :: Monad m => SP (AOT_sp m) -> AOT_sp m a -> m a
runAOT_sp sp c = liftM fst $ run_sp c ([], sp)

-- | Monadic weaver
weavet_sp :: (Typeable1Monad m) => 
            (a -> AOT_sp m b) -> AspectEnv (AOT_sp m) -> SP (AOT_sp m) -> Jp (AOT_sp m) a b -> m (a -> AOT_sp m b)
weavet_sp f [] (SP _ []) _ = return f 
weavet_sp f aenv@(asp:asps) sp@(SP pcG []) jp = 
 case asp of EAspect (Aspect pc adv _) -> do 
                                             (match, _) <- run_sp (runPC pcG jp) (aenv, sp)
                                             if match
                                                then return f  
                                                else do (match', _) <- run_sp (runPC pc jp) (aenv, sp)
                                                        weavet_sp (if match'  
                                                                     then apply_adv adv f
                                                                     else f) asps sp jp
weavet_sp f aenv sp@(SP pcG aenvG@(asp:asps)) jp = 
 case asp of EAspect (Aspect pc adv _) -> do 
                                            (match, _) <- run_sp (runPC pc jp) (aenv, sp)
                                            weavet_sp (if match  
                                                          then apply_adv adv f
                                                          else f) aenv (SP pcG asps) jp


-- | Implementation of woven application for AOT_sp, used in the overloading of #.
-- FunctionTag argument is used for function identity:
--     Wrapped functions are equal when their tags are equal
--     Regular functions share the same tag, and are compared using StableNames.
wappt_sp :: (Typeable1Monad m, PolyTypeable (a -> AOT_sp m b)) => 
           (a -> AOT_sp m b) -> FunctionTag -> a -> AOT_sp m b
wappt_sp f t a = mkAOT_sp $ \ (aenv, sp) -> do
                 woven_f <- weavet_sp f aenv sp (newjp f t a)
                 run_sp (woven_f a) (aenv, sp)

-- | Every regular functions is tagged with the same default tag.
instance Typeable1Monad m => OpenApp (->) (AOT_sp m) where
         f # a = wappt_sp f defaultFunctionTag a

-- | Function is a wrapper to add a notion of identity based on tags.
instance Typeable1Monad m => OpenApp Function (AOT_sp m) where
         (Function f t) # a = wappt_sp f t a

-- | Typeable instance so types of computations in AOT_sp can be compared (like in pcCall and pcType)
instance Typeable1Monad m => Typeable1 (AOT_sp m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "AOT_sp" "AOT_sp" "AOT_sp") [typeOf1 (undefined :: m ())]

-- | The semantics of aspect deployment are defined in the
-- MonadDeploy typeclass. AOT_sp assumes it is on top of an MonadDeploy
-- instance, and uses that functions for aspect deployment.
instance (Typeable1Monad m, MonadDeploy AOT_sp m) => AOPMonad (AOT_sp m) where
      deploy asp   = mkAOT_sp $ \ (aenv, pc) -> do
                     aenv' <- deployInEnv asp aenv
                     return ((), (aenv',pc))
      undeploy asp = mkAOT_sp $ \ (aenv, pc) -> do
                     aenv' <- undeployInEnv asp aenv
                     return ((), (deleteAsp (EAspect asp) aenv, pc))


-- Interaction with standard monad transformers

data RPair s a = RPair a s
instance Functor (RPair s) where
  fmap f (RPair a s) = RPair (f a) s

toRP   (a,s)       = RPair a s
fromRP (RPair a s) = (a,s)

instance MonadTrans AOT_sp where
         lift ma = mkAOT_sp $ \ (aenv, pc) -> do
                   a <- ma
                   return (a, (aenv,pc))
         mt = MT
         unlift f = mkAOT_sp $ \ (aenv, pc) ->
                    f (unsafeCoerce (\m -> run_sp m (aenv, pc) >>= return . toRP)) >>= return . fromRP         

instance (MonadState s m) => MonadState s (AOT_sp m) where
         get = lift get
         put = lift . put

instance (MonadError s m) => MonadError s (AOT_sp m) where
         throwError = lift . throwError
         ma `catchError` h = mkAOT_sp $ \ (aenv, pc) ->
            run_sp ma (aenv, pc) `catchError` \e -> run_sp (h e) (aenv, pc)

instance (MonadWriter w m) => MonadWriter w (AOT_sp m) where
    tell     = lift . tell
    listen m = mkAOT_sp $ \ (aenv, pc) -> do
               ((a, ctx), w) <- listen (run_sp m (aenv, pc))
               return ((a, w), ctx)

    pass m = mkAOT_sp $ \ (aenv, pc) -> pass $ do
             ((a, f), ctx) <- run_sp m (aenv, pc)
             return ((a, ctx), f)

instance (MonadReader r m) => MonadReader r (AOT_sp m) where
    ask       = lift ask
    local f m = mkAOT_sp $ \ (aenv, pc) -> local f (run_sp m (aenv, pc))



