{-# LANGUAGE TemplateHaskell,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses,
             DeriveDataTypeable,
             RankNTypes,
             NoMonomorphismRestriction
 #-}

module AOP.Examples.Fib2 (fib, ppcFib)
where

import AOP.Default

fibTag = $newTag

fibBase :: (Typeable1Monad m, Num a, Num b) => Function a (m b) 
fibBase = mkFunction (\n_ -> return 1) fibTag

fibPc = pcCall fibBase `pcAnd` pcArgGT 2

ppcFib :: (Typeable1Monad m, Num a1, Num b1, Ord a1, LessGen (a1 -> m b1) (a2 -> m b2)) =>
          ProtectedPC m a1 (m b1) (Narrow a2 b2 c m) a2 b2
ppcFib = protectPC fibPc narrow

fibAdv proceed n = do f1 <- fibBase # (n-1)
                      f2 <- fibBase # (n-2)
                      return (f1 + f2)
                   
fib = do deploy (aspect fibPc fibAdv)
         return errorFib 
      where errorFib n = if n < 0 
                         then throwError "Not defined on negative value"
                         else fibBase # n

runMemo c = runIdentity (runErrorT (runAOT c))

program n = runMemo $ do f <- fib
                         f # n
