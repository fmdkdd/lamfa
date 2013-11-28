{-# LANGUAGE TemplateHaskell,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses,             
             DeriveDataTypeable,
             RankNTypes,
             NoMonomorphismRestriction
 #-}

module AOP.Examples.Fib (fib, pcFib) where
import AOP.Default

fibTag = $newTag

fibBase :: Typeable1Monad m => Function Int (m Int) 
fibBase = mkFunction (\n -> return 1) fibTag

pcFib = pcCall fibBase `pcAnd` pcArgGT 2

fibAdv proceed n = do f1 <- fibBase # (n-1)
                      f2 <- fibBase # (n-2)
                      return (f1 + f2)
                   
fib = do deploy (aspect pcFib fibAdv)
         return (fibBase #)