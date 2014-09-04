{-# LANGUAGE ScopedTypeVariables #-}


module A where

import AOP.Default

f :: Typeable1Monad m => Int -> m a
f = undefined

g :: Typeable1Monad m => Int -> m Float
g = undefined

pc1 :: Typeable1Monad m => PC m Int a
pc1 = pcType f

pc2 :: Typeable1Monad m => PC m Int Float
pc2 = pcCall g
