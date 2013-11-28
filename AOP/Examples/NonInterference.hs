{-# LANGUAGE TemplateHaskell,
             FlexibleContexts,
             NoMonomorphismRestriction,
             RankNTypes,
             ImpredicativeTypes,
             GeneralizedNewtypeDeriving,
             TypeOperators,
             KindSignatures,
             ConstraintKinds,
             ScopedTypeVariables
 #-}

module MemoizedFib4 where
import AOP.Default
import Data.Map
import Control.Monad.Mask
import AOP.Internal.NIAOT2

-- =============================================================================
-- Fibonacci module

fibTag = $newTag

fibBase :: Typeable1Monad m => Function Int (m Int) 
fibBase = mkFunction (\n -> return 1) fibTag

pcFib = pcCall fibBase `pcAnd` pcArgGT 2

fibAdv proceed n = do f1 <- fibBase # (n-1)
                      f2 <- fibBase # (n-2)
                      return (f1 + f2)

fib = do deploy (aspect pcFib fibAdv)
         return $ (fibBase #)

-- =============================================================================
-- | Enforcing non-interference between advices using parametricity
niAdviceL1 :: (
    Monad m,
    MonadTrans t1,
    MonadTrans t2,
    Typeable1Monad (t2 m),
    Typeable1Monad (t1 (t2 m))) => NIAdviceL1 t1 a b -> Advice (NIAOT2 t1 t2 m) a b
niAdviceL1 adv = adv

type NIAdviceL1 t1 a b = forall t2 m. (
     Monad m,
     MonadTrans t1,
     MonadTrans t2,
     Typeable1Monad (t2 m),
     Typeable1Monad (t1 (t2 m))) => Advice (NIAOT2 t1 t2 m) a b

type NIAdviceL2 t2 a b = forall t1 m. (
     Monad m,
     MonadTrans t1,
     MonadTrans t2,
     Typeable1Monad (t2 m),
     Typeable1Monad (t1 (t2 m)))
     => Advice (NIAOT2 t1 t2 m) a b

niAdviceL2 :: (
     Monad m,
     MonadTrans t1,
     MonadTrans t2,
     Typeable1Monad (t2 m),
     Typeable1Monad (t1 (t2 m))) => NIAdviceL2 t2 a b -> Advice (NIAOT2 t1 t2 m) a b
niAdviceL2 adv = adv

checkArgNI proceed arg = if arg < 0
                            then (niLift2 . throwError) "Error: negative argument"
                            else proceed arg

memoNI proceed n =
     do m <- niLift2 $ lift $ get
        if member n m
           then return (m ! n)
           else do y <- proceed n
                   m' <- niLift2 $ lift $ get
                   (niLift2 . lift . put) (insert n y m')
                   return y

type S = NIAOT2 (ErrorT String) (StateT (Map Int Int)) Identity

runS :: S a -> Either String a
runS c = runIdentity $ evalStateT (runErrorT $ (runNIAOT2 c)) empty

programNI :: Int -> S Int
programNI n = do deploy (aspect pcFib (niAdviceL2 memoNI))
                 f <- fib                            
                 deploy (aspect (pcCall f) (niAdviceL1 checkArgNI))
                 f # n


-- ================================================================================
-- Using structural masks

type S' = AOT (ErrorT String (StateT (Map Int Int) Identity))

runS' :: S' a -> Either String a
runS' c = runIdentity $ evalStateT (runErrorT $ (runAOT c)) empty

checkArgNI' proceed arg = if arg < 0
                             then throwError "Error: negative argument"
                             else proceed arg

memoNI' :: (Monad m, Ord a, MonadState (Map a b) m) => Advice m a b
memoNI' proceed n =
     do m <- get
        if member n m
           then return (m ! n)
           else do y <- proceed n
                   m' <- get
                   put (insert n y m')
                   return y

fibMemoErr' n = 
  do deploy (aspect pcFib (withView v1 memoNI'))
     f <- fib
     deploy (aspect (pcCall f) (withView v2 checkArgNI'))
     f # n
  where v1 = o `vcomp` i
        v2 = o `vcomp` i

data StateTag = StateTag
data ErrorTag = ErrorTag

fibMemoErr'' :: forall m n n'. (
   Typeable1Monad m,
   Typeable1Monad n,
   Typeable1Monad n',
   TWith StateTag n (AOT m),
   TWith ErrorTag n' (AOT m), 
   MonadState (Map Int Int) n,
   MonadError String n')
   => Int -> (AOT m) Int
fibMemoErr'' n = 
  do deploy (aspect pcFib (withView v1 memoNI'))
     f <- fib
     deploy (aspect (pcCall f) (withView v2 checkArgNI'))
     f # n
  where v1 = structure StateTag :: n  :><: (AOT m)
        v2 = structure ErrorTag :: n' :><: (AOT m)

-- ================================================================================
-- Using the AdviceCombinator class constraint

fibMemoErr3 :: forall m n n'. (
  Typeable1Monad m,
  Monad n,
  Monad n',
  OpenApp (->) m,
  AOPMonad m,
  TWith StateTag n m,
  MonadState (Map Int Int) n,
  TWith ErrorTag n' m,
  MonadError String n')
  => Int -> m Int
fibMemoErr3 n = do
   deploy (aspect pcFib
                  (toAdvice (RestrictedAdv v1 CombinationAdvTag)
                            (CombinationAdv memoNI')))
   f <- fib
   deploy (aspect (pcCall f)
                  (toAdvice (RestrictedAdv v2 CombinationAdvTag)
                            (CombinationAdv checkArgNI')))
   f # n
 where v1 = structure StateTag :: n  :><: m
       v2 = structure ErrorTag :: n' :><: m


narrowingMemoNI = (p, (bef, aft), rep) where
     p x       = do {m <- get; return (not (member x m))}
     bef _     = return ()
     aft x r _ = do {m <- get; put (insert x r m)}
     rep x     = do {m <- get; return (m ! x)}

augmentationCheckArgNI = beforeAdv $ \ arg -> if arg < 0
                             then throwError "Error: negative argument"
                             else return ()

fibMemoErr4 :: forall m n n'. (
  Typeable1Monad m,
  Monad n,
  Monad n',
  OpenApp (->) m,
  AOPMonad m,
  TWith StateTag n m,
  MonadState (Map Int Int) n,
  TWith ErrorTag n' m,
  MonadError String n')
  => Int -> m Int
fibMemoErr4 n =  do
   deploy (aspect pcFib
                  (toAdvice (RestrictedAdv v1 NarrowingAdvTag)
                            (NarrowingAdv narrowingMemoNI)))
   f <- fib
   deploy (aspect (pcCall f)
                  (toAdvice (RestrictedAdv v2 AugmentationAdvTag)
                            (AugmentationAdv augmentationCheckArgNI)))
   f # n
 where v1 = structure StateTag :: n  :><: m
       v2 = structure ErrorTag :: n' :><: m

runFibMemoErr3 n = runM (fibMemoErr3 n)

runFibMemoErr4 n = runM (fibMemoErr4 n)

type M = AOT (TErrorT ErrorTag String (TStateT StateTag (Map Int Int) Identity))
runM :: M a -> Either String a
runM c = runIdentity $ evalTStateT StateTag empty $ runTErrorT ErrorTag $ (runAOT c)