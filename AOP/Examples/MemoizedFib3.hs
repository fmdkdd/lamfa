{-# LANGUAGE FlexibleContexts,
             ImpredicativeTypes,
             NoMonomorphismRestriction
 #-}

module MemoizedFib3 where -- (fib) where
import qualified AOP.Examples.Fib3 as Fib
import AOP.Default
import Data.Map
import Control.Monad.Error

import Debug.Trace

memo :: Ord a => NIAdvice (StateT (Map a b)) a b
memo proceed n =
     do m <- get
        if member n m
        then return (m ! n)
        else do y <- proceed n
                m' <- get
                put (insert n y m')
                return y

memoWrong :: NIAdvice (ErrorT String) Integer Integer
memoWrong proceed n =
     do trace "asdadsa" (proceed (-n)) `catchError` (\e -> return 9999)

fib = let preAspect = pAspect Fib.ppcFib --makes inference work
      in do deploy (preAspect memo)
            Fib.fib 

fib2 = let preAspect = pAspect Fib.ppcFib --makes inference work
       in do deploy (preAspect memoWrong)
             Fib.fibWrong



runMemo c = runIdentity (runErrorT (evalStateT (runNIAOT c) empty))

-- program n = runMemo $ do f <- fib
--                          f # n


runMemo2 c = runIdentity (runErrorT $ runErrorT (runNIAOT c))
program2 n = runMemo2 $ do f <- fib2
                           f # n
