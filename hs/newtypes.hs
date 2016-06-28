

-- class Monad m => Plus m where
--   evalPlus :: m Int

data Term = Const Int
          | Plus Term Term


eval :: (Monad m) => Term -> m Int
eval (Const n) = do
  return n
eval (Plus l r) = do
  v1 <- eval l
  v2 <- eval r
  return (v1 + v2)

-- Ok, so putting a monad around only allows us to use the `do` notation so
-- far.  Not tremendously useful.

-- I want to change the way `eval` behaves inside `eval`.  More generally, I
-- want to be able to change the meaning of names inside a piece of code.

-- class Monad m => EvalMonad m where
--   eval :: Term -> m Int

-- This ^ doesn't work because `eval` is already defined.
