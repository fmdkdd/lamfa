{-# Language FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             KindSignatures,
             GeneralizedNewtypeDeriving
 #-}

module AOP.Internal.NIAOT2 (
 NIAOT2,
 runNIAOT2,
 niLift2
) where

import AOP.Internal.Typeable1Monad
import AOP.Internal.Aspect
import AOP.Internal.Pointcut
import AOP.Internal.Advice
import AOP.Internal.Joinpoint
import AOP.Internal.AOPMonad
import AOP.Internal.OpenApp
import AOP.Internal.Function


{- 
Non-Interference AOT transformer.
The NIAOT is like the AOT transformer, but it enforces a splitting on the monadic stack.
Base code and aspect code have access to a different part of the stack.
-}


-- | The splitting is done by assuming the stack is constructed by a transformer t on top of a monad m.
newtype NIAOT2 (t1 :: (* -> *) -> * -> *)
               (t2 :: (* -> *) -> * -> *)
               m a = NIAOT2 { unNIAOT2 :: StateT (AspectEnv (NIAOT2 t1 t2 m)) (t1 (t2 m)) a }
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)

runNI2 = runStateT . unNIAOT2
mkNIAOT2 = NIAOT2 . StateT

-- | Gets the underlying computation
runNIAOT2 :: (Monad (t1 (t2 m))) => NIAOT2 t1 t2 m a -> (t1 (t2 m)) a
runNIAOT2 c = liftM fst $ runNI2 c []

--  Monadic weaver
weavet :: (Typeable1Monad (t2 m), Typeable1Monad (t1 (t2 m))) =>
          (a -> NIAOT2 t1 t2 m b) -> AspectEnv (NIAOT2 t1 t2 m) -> 
          AspectEnv (NIAOT2 t1 t2 m) -> Jp (NIAOT2 t1 t2 m) a b ->
          (t1 (t2 m)) (a ->  NIAOT2 t1 t2 m b, AspectEnv (NIAOT2 t1 t2 m))
weavet f [] fenv _ = return (f, fenv)
weavet f (asp:asps) fenv jp = 
    case asp of EAspect (Aspect pc adv _) -> 
                            do                               
                               (match, fenv') <- runNI2 (runPC pc jp) fenv
                               weavet (if match
                                        then apply_adv adv f
                                        else f)
                                       asps fenv' jp

-- | Implementation of woven application for NIAOT, used in the overloading of #.
-- | Function tags are incorporated like in AOT.
wappt :: (Typeable1Monad (t2 m), Typeable1Monad (t1 (t2 m)), PolyTypeable (a -> NIAOT2 t1 t2 m b)) => 
         (a -> NIAOT2 t1 t2 m b) -> FunctionTag -> a -> NIAOT2 t1 t2 m b
wappt f t a = mkNIAOT2 $ \ aenv -> do
              (woven_f, fenv) <- weavet f aenv aenv (newjp f t a)
              runNI2 (woven_f a) fenv

-- | Open application of regular functions.
instance (Typeable1Monad (t2 m), Typeable1Monad (t1 (t2 m))) => OpenApp (->) (NIAOT2 t1 t2 m) where
         f # a = wappt f defaultFunctionTag a

-- | Open application of tagged functions.
instance (Typeable1Monad (t2 m), Typeable1Monad (t1 (t2 m))) => OpenApp Function (NIAOT2 t1 t2 m) where
         (Function f t) # a = wappt f t a

-- | Lift a computation from the underlying monad into NIAOT.
niLift2 ma = mkNIAOT2 $ \ aenv -> do
            a <- ma
            return (a, aenv)

-- | NIAOT performs top-level deployment of aspects
instance (Typeable1Monad (t2 m), Typeable1Monad (t1 (t2 m))) => AOPMonad (NIAOT2 t1 t2 m) where
      deploy asp   = mkNIAOT2 $ \ aenv -> return ((), EAspect asp : aenv)
      undeploy asp = mkNIAOT2 $ \ aenv -> return ((), deleteAsp (EAspect asp) aenv)

-- | Support for PolyTypeable
instance (Typeable1Monad (t2 m),Typeable1Monad (t1 (t2 m))) => Typeable1 (NIAOT2 t1 t2 m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "NIAOT2" "NIAOT2" "NIAOT2") 
                     [typeOf1 (undefined :: (t1 (t2 m)) ())]


-- Interaction with standard monad transformers

instance (MonadState s (t1 (t2 m))) => MonadState s (NIAOT2 t1 t2 m) where
         get = niLift2 get
         put = niLift2 . put

instance (MonadError s (t1 (t2 m))) => MonadError s (NIAOT2 t1 t2 m) where
         throwError = niLift2 . throwError
         ma `catchError` h = mkNIAOT2 $ \ aenv ->
            runNI2 ma aenv `catchError` \e -> runNI2 (h e) aenv

instance (MonadWriter w (t1 (t2 m))) => MonadWriter w (NIAOT2 t1 t2 m) where
    tell     = niLift2 . tell
    listen m = mkNIAOT2 $ \ aenv -> do
               ((a, aenv'), w) <- listen (runNI2 m aenv)
               return ((a, w), aenv')
    pass m   = mkNIAOT2 $ \ aenv -> pass $ do
               ((a, f), aenv') <- runNI2 m aenv
               return ((a, aenv'), f)

instance (Monad m, MonadReader r (t1 (t2 m))) => MonadReader r (NIAOT2 t1 t2 m) where
    ask       = niLift2 ask
    local f m = mkNIAOT2 $ \s -> local f (runNI2 m s)
