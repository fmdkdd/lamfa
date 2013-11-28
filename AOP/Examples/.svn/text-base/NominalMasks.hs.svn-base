{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             ScopedTypeVariables
  #-}

module NominalMasks where

import Control.Monad.Views
import Control.Monad.Mask
import Control.Monad.Identity
import Control.Monad.State

data Counter1  = Counter1
data Counter2  = Counter2

inc :: MonadState Int m => m Int
inc = do  x <- get
          put (x + 1)
          return (x + 1)

-- A pattern type signature (ie. using the do) cannot bind scoped type
-- variables unless the pattern has a rigid type context.

-- Using the "forall" is exactly what we want to fix this problem as
-- it puts the type variable in scope throughout the definition of the
-- function.

c :: forall n1 n2 m. (Monad m, MonadState Int n1, MonadState Int n2,  
                      TWith Counter1 n1 m, TWith Counter2 n2 m) => m ()
c = do  (inc :: n1 Int)        `use` Counter1
        (inc >> inc :: n2 Int) `use` Counter2
        return ()

c' :: forall n1 n2 m. (Monad m, MonadState Int n1, MonadState Int n2,  
                      TWith Counter1 n1 m, TWith Counter2 n2 m) => m ()
c' = do  use1 inc
         use2 (inc >> inc)
         return ()
     where use1 = flip use Counter1 :: n1 a -> m a
           use2 = flip use Counter2 :: n2 a -> m a


test1 = runIdentity $ runTStateT Counter2 5 $ runTStateT Counter1 0 $ c
test2 = runIdentity $ runTStateT Counter1 0 $ runTStateT Counter2 5 $ c

test3 = runIdentity $ runTStateT Counter2 5 $ runTStateT Counter1 0 $ c'
test4 = runIdentity $ runTStateT Counter1 0 $ runTStateT Counter2 5 $ c'


doubleInc2  :: (MonadMorphism (~>), MonadState Int n1 
               ,MonadState Int n2, Monad m) 
            => (n1 ~> m) -> (n2 ~> m) -> m Int
doubleInc2 v1 v2  = do  from v1 inc
                        from v2 inc

f = (case of 1 -> "One"; - -> "Not One")