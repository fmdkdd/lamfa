{-# LANGUAGE TemplateHaskell,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses,
             DeriveDataTypeable,
             ImpredicativeTypes,
             NoMonomorphismRestriction
 #-}

module AOP.Examples.Fib3 (fib, ppcFib) where

import AOP.Default

fibTag = $newTag

pcFib = pcCall fibBase `pcAnd` pcArgGT 2

-- | Because the protected pointcut requires NIAdvice, and
-- by the definition of fib external advice cannot
-- catch exceptions raised by errorFib.
ppcFib :: (Typeable1Monad m, Typeable1Monad (t m), LessGen (Integer -> (NIAOT t m) Integer) (a -> NIAOT t m b)) =>
           ProtectedPC (NIAOT t m) Integer ((NIAOT t m) Integer) (NIAdvice t a b) a b
ppcFib = protectPC pcFib niAdvice

fibBase :: Typeable1Monad m => Function Integer (m Integer) 
fibBase = mkFunction (\ _ -> return 1) fibTag

fibAdv :: (MonadTrans t, Typeable1Monad (t m), MonadError String m) => a -> Integer -> NIAOT t m Integer
fibAdv proceed n = do f1 <- errorFib # (n-1)
                      f2 <- errorFib # (n-2)
                      return (f1 + f2)

-- | The fib function requires the MonadError String effect in the part of the stack available to base programs
-- fib :: (Typeable1Monad m, Typeable1Monad (t m), MonadTrans t, MonadError String m) =>
--         NIAOT t m (Integer -> NIAOT t m Integer)
fib = do deploy (aspect pcFib fibAdv)
         return errorFib

errorFib :: (MonadTrans t, Typeable1Monad (t m), MonadError String m) => (Integer -> NIAOT t m Integer)
errorFib n = if n < 0 
                then (niLift . lift . throwError) "Not defined on negative value"
                else fibBase # n

runMemo c = runIdentity (runErrorT $ runIdentityT (runNIAOT c))

program n = runMemo $ do f <- fib
                         f # n
