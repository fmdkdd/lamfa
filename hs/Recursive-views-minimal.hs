{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foo where

import "mzv" Control.Monad.State
import "mzv" Control.Monad.Identity
import Control.Monad.Mask
import Control.Monad.Views

data Tag = Tag

type M = TStateT Tag Integer Identity

runM :: M a -> a
runM c = runIdentity $ evalTStateT 0 c

foo :: forall m n. (Monad m, TWith Tag n m, MonadState Integer n)
       => Integer -> m Integer
foo 0 = return 0
foo x = foo (x - 1)

main :: IO ()
main = print $ runM $ foo 42
