{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

import "mtl" Control.Monad.State

type M a = (Int, a)

instance Monad ((,) Int) where
    (n, x) >>= f = let (n', x') = f x
                   in (n'+n, x')
    return x = (0, x)

eval :: String -> M String
eval s = return s

instance Monad ((,) Char) where
    (c, x) >>= f = let (c', x') = f x
                   in (c', x')
    return x = (' ', x)

type M' a = (Char, M a)

eval' :: String -> M' String
eval' s = return (eval s)

-- get :: M Int
-- get = (n, n)

-- put :: M ()
-- put n = \_ -> (n, ())

--incr :: M a
-- incr = get >>= put . (+1)


type N = State Int

f :: a -> N a
f = return . id

run :: a -> (a, Int)
run a = runState (f a) 0

type N' = StateT Int N

f' :: a -> N' a
f' a = lift (f a)

run' :: a -> ((a, Int), Int)
run' a = runState (runStateT (f' a) 0) 1
