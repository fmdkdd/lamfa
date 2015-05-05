{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foo where

import "mzv" Control.Monad.State
import "mzv" Control.Monad.Identity
import Control.Monad.Mask
import Control.Monad.Views

data AccTag = AccTag

type M = TStateT AccTag Integer Identity

runM :: M a -> (a, Integer)
runM c = runIdentity $ runTStateT 0 c

foo :: forall m n t. (Monad m, MonadTrans t, m ~ t n, TWith AccTag n m, MonadState Integer n)
       => Integer -> m Integer
foo 0 = return 0
foo x = do acc <- getv viewAcc
           putv viewAcc (acc + x)
           foo (x - 1)
        where viewAcc = structure AccTag :: n :><: m

main :: IO ()
main = print $ runM $ foo 42
