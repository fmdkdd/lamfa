{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foo where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Mask
import Control.Monad.Views

data AccTag = AccTag

type M = TStateT AccTag Integer Identity

runM :: M a -> (a, Integer)
runM c = runIdentity $ runTStateT 0 c

foo :: forall m n. (Monad m, TWith AccTag n m, MonadState Integer n)
       => Integer -> m (Integer, n ())
foo 0 = return (0, return ())
foo x = do (v, _) <- foo (x - 1) :: m (Integer, n ())
           acc <- getv viewAcc
           putv viewAcc (acc + v)
           return (v, return ())
        where viewAcc = structure AccTag :: n :><: m

-- Discard the dummy computation
fooWrapper :: forall m n. (Monad m, TWith AccTag n m, MonadState Integer n)
              => Integer -> m Integer
fooWrapper x = do (v, _) <- foo x :: m (Integer, n ())
                  return v

main :: IO ()
main = print $ runM $ fooWrapper 10
