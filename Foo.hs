{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foo where

import "mzv" Control.Monad.State
import "mzv" Control.Monad.Identity
import "mzv" Control.Monad.Mask
import "mzv" Control.Monad.Views

data EnvTag   = EnvTag
data StoreTag = StoreTag

-- TErrorT, TWriterT, and so on...
type M = TStateT EnvTag Integer (TStateT StoreTag [Integer] Identity)

runM :: M a -> (a, [Integer])
runM c = runIdentity $ runTStateT [] (evalTStateT 0 c)

foo :: forall m n. (Monad m, TWith StoreTag n m, MonadState [Integer] n)
       => Integer -> m (Integer, n ())
foo 0 = fooWrap 0
foo n = do r <- (foo (n - 1)) :: m (Integer, n ())
           store <- getv viewStore
           putv viewStore (n:store)
           return (0, return ())
        where viewStore = structure StoreTag :: n :><: m


fooWrap v = return (v, return ())

fooWrapper :: forall m n. (Monad m, TWith StoreTag n m, MonadState [Integer] n)
              => Integer -> m Integer
fooWrapper n = do (i, _) <- foo n :: m (Integer, n ())
                  return i

main :: IO ()
main = print $ runM $ fooWrapper 42
