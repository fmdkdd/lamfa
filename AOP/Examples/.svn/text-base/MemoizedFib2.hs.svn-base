{-# LANGUAGE FlexibleContexts,
             NoMonomorphismRestriction    
 #-}

module MemoizedFib2 (fib) where

import qualified AOP.Examples.Fib2 as Fib
import AOP.Default
import Data.Map
import Control.Monad.Error

-- Memoization as "wild" advice
memo proceed n =
     do m <- get
        if member n m
        then return (m ! n)
        else do y <- proceed n
                m' <- get
                put (insert n y m')
                return y

-- Memoization as narrowing advice
memo' :: (MonadState (Map a b) m, Ord a) => Narrow a b () m
memo' = (p, (bef, aft), rep) where
     p x       = do {m <- get; return (not (member x m))}
     bef _     = return ()
     aft x r _ = do {m <- get; put (insert x r m)}
     rep x     = do {m <- get; return (m ! x)}

fib :: (Num b, Num a, Ord a, MonadError String m, Typeable1 m, (MonadState (Map a b) m)) =>
       AOT m (a -> AOT m b)
-- does not typecheck
-- fib = do deploy (aspect Fib.ppcFib memo) -- ppcFib is not a PC
--          Fib.fib
-- fib = do deploy (pAspect Fib.ppcFib memo) -- memo is not narrowing
--          Fib.fib
--- works with memo'
fib = do deploy (pAspect Fib.ppcFib memo')
         Fib.fib

runMemo c = runIdentity (runErrorT (evalStateT (runAOT c) empty))

program n = runMemo $ do f <- fib
                         f # n