{-# LANGUAGE TypeOperators
  #-}

module AOP.Internal.WithView (
withView,
-- withViewJP,
-- withViewPC,
) where

import Control.Monad.Views
import AOP.Internal.Advice (Advice)

withView :: (Monad n, Monad m) => n :><: m -> Advice n a b -> Advice m a b
withView v adv proceed arg = from v $ adv (\ a -> to v (proceed a)) arg

-- TO DO: withViewJP and withViewPC