{-# Language FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             KindSignatures,
             GeneralizedNewtypeDeriving
 #-}

module AOP.Internal.NIAOT (
 NIAOT,
 runNIAOT,
 niLift
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
newtype NIAOT (t :: (* -> *) -> * -> *) m a = NIAOT { unNIAOT :: StateT (AspectEnv (NIAOT t m)) (t m) a }
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)
runNI = runStateT . unNIAOT
mkNIAOT = NIAOT . StateT

-- | Gets the underlying computation
runNIAOT :: (Monad (t m)) => NIAOT t m a -> t m a
runNIAOT c = liftM fst $ runNI c []

--  Monadic weaver
weavet :: Typeable1Monad (t m) =>
          (a -> NIAOT t m b) -> AspectEnv (NIAOT t m) -> 
          AspectEnv (NIAOT t m) -> Jp (NIAOT t m) a b ->
          t m (a ->  NIAOT t m b, AspectEnv (NIAOT t m))
weavet f [] fenv _ = return (f, fenv)
weavet f (asp:asps) fenv jp = 
    case asp of EAspect (Aspect pc adv _) -> 
                            do                               
                               (match, fenv') <- runNI (runPC pc jp) fenv
                               weavet (if match
                                        then apply_adv adv f
                                        else f)
                                       asps fenv' jp

-- | Implementation of woven application for NIAOT, used in the overloading of #.
-- | Function tags are incorporated like in AOT.
wappt :: (Typeable1Monad (t m), PolyTypeable (a -> NIAOT t m b)) => 
         (a -> NIAOT t m b) -> FunctionTag -> a -> NIAOT t m b
wappt f t a = mkNIAOT $ \ aenv -> do
              (woven_f, fenv) <- weavet f aenv aenv (newjp f t a)
              runNI (woven_f a) fenv

-- | Open application of regular functions.
instance Typeable1Monad (t m) => OpenApp (->) (NIAOT t m) where
         f # a = wappt f defaultFunctionTag a

-- | Open application of tagged functions.
instance Typeable1Monad  (t m) => OpenApp Function (NIAOT t m) where
         (Function f t) # a = wappt f t a

-- | Lift a computation from the underlying monad into NIAOT.
niLift ma = mkNIAOT $ \ aenv -> do
            a <- ma
            return (a, aenv)

-- | NIAOT performs top-level deployment of aspects
instance (Typeable1Monad (t m)) => AOPMonad (NIAOT t m) where
      deploy asp   = mkNIAOT $ \ aenv -> return ((), EAspect asp : aenv)
      undeploy asp = mkNIAOT $ \ aenv -> return ((), deleteAsp (EAspect asp) aenv)

-- | Support for PolyTypeable
instance (Typeable1Monad (t m)) => Typeable1 (NIAOT t m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "NIAOT" "NIAOT" "NIAOT") 
                     [typeOf1 (undefined :: t m ())]


-- Interaction with standard monad transformers

instance (MonadState s (t m)) => MonadState s (NIAOT t m) where
         get = niLift get
         put = niLift . put

instance (MonadError s (t m)) => MonadError s (NIAOT t m) where
         throwError = niLift . throwError
         ma `catchError` h = mkNIAOT $ \ aenv ->
            runNI ma aenv `catchError` \e -> runNI (h e) aenv

instance (MonadWriter w (t m)) => MonadWriter w (NIAOT t m) where
    tell     = niLift . tell
    listen m = mkNIAOT $ \ aenv -> do
               ((a, aenv'), w) <- listen (runNI m aenv)
               return ((a, w), aenv')
    pass m   = mkNIAOT $ \ aenv -> pass $ do
               ((a, f), aenv') <- runNI m aenv
               return ((a, aenv'), f)

instance (Monad m, MonadReader r (t m)) => MonadReader r (NIAOT t m) where
    ask       = niLift ask
    local f m = mkNIAOT $ \s -> local f (runNI m s)
