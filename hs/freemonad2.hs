{-# LANGUAGE DeriveFunctor #-}

-- import Control.Monad.Free

-- Less type headaches with the following definition:

data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

liftF :: (Functor f) => f r -> Free f r
liftF cmd = Free (fmap Pure cmd)


data Term next = Const Int next
                 -- | Lam String next
                 -- | App String next
               deriving (Functor, Show)

type Prog = Free Term

-- cons :: Int -> Free Term a
cons n = liftF (Const n id)

-- prg :: Prog
prg = do
  x <- var 10
  x <- cons 10
  return x

-- eval :: Prog r -> r
eval (Free (Const i n)) = i
eval (Pure r) = r

-- data DSL next = Get String (String -> next)
--               | End
--               deriving Functor

-- get key = liftF (Get key id)
-- end = liftF End



-- -- prg1 :: Free DSL a
-- prg1 = do foo <- get "foo"
--           return ()
