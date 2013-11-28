module AOP.Semantics.EL.MonadEL (Level, MonadEL(..)) where

type Level = Int

-- | A level-aware monad can perform level-shifting operations.
class Monad m => MonadEL m where
  current :: m Level
  up :: m a -> m a
  down :: m a -> m a
  lambda_at :: (a -> m b) -> Level -> a -> m b