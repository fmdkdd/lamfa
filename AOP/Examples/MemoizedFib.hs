{-# LANGUAGE FlexibleContexts,
             NoMonomorphismRestriction
 #-}

module MemoizedFib (fib) where
import qualified AOP.Examples.Fib as Fib
import AOP.Default
import Data.Map
import Control.Monad.Error

memo proceed n =
     do m <- get
        if member n m
        then return (m ! n)
        else do y <- proceed n
                m' <- get
                put (insert n y m')
                return y

fib :: (Monad m, Num b, Num a, Ord a, Typeable1 m, Typeable a, Typeable b, MonadError String m) =>
     NIAOT (StateT (Map a b)) m (a -> NIAOT (StateT (Map a b)) m b)
fib = do deploy (aspect Fib.pcFib memo)
         Fib.fib

runMemo c = runIdentity (runErrorT (evalStateT (runNIAOT c) empty))

program n = runMemo $ do f <- fib
                         f # n


--------------------------------------------------------------------------------
-- Memoization as narrowing advice

memo' = (p, (bef, aft), rep) where
     p x       = do {m <- get; return (not (member x m))}
     bef _     = return ()
     aft x r _ = do {m <- get; put (insert x r m)}
     rep x     = do {m <- get; return (m ! x)}

fib' :: (Typeable1Monad m, Num a, Num b, Ord a, Typeable a, Typeable b, MonadError String m) =>
     NIAOT (StateT (Map a b)) m (a -> NIAOT (StateT (Map a b)) m b)
fib' = do deploy (aspect Fib.pcFib (narrow memo'))
          Fib.fib

program' n = runMemo $ do f <- fib'
                          f # n
