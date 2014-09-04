{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foo where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Mask
import Control.Monad.Views

data Tag = Tag

type M = TStateT Tag Integer Identity

runM :: M a -> a
runM c = runIdentity $ evalTStateT 0 c

foo :: forall m n. (Monad m, TWith Tag n m, MonadState Integer n)
       => Integer -> m (Integer, n ())
foo 0 = return (0, return ())
foo x = foo (x - 1)

-- Discard the dummy computation
fooWrapper :: forall m n. (Monad m, TWith Tag n m, MonadState Integer n)
              => Integer -> m Integer
fooWrapper x = do (v, _) <- foo x :: m (Integer, n ())
                  return v

main :: IO ()
main = print $ runM $ fooWrapper 42
