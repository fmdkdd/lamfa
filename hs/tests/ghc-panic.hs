{-# LANGUAGE PackageImports #-}

import "mtl" Control.Monad.State

--type M = State ()

f :: a -> a -> State () a
f = undefined

--type M' = StateT () M

f' :: a -> a -> StateT () (State ()) a
f' = lift . f
