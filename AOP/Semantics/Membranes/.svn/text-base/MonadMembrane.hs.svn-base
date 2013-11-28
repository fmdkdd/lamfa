{-# LANGUAGE MultiParamTypeClasses,
             RankNTypes,
             FlexibleContexts,
             ImpredicativeTypes,
             TypeOperators
 #-}

module AOP.Semantics.Membranes.MonadMembrane (MonadMembrane(..)) where

import AOP.Internal.Typeable1Monad
import AOP.Internal.Joinpoint
import AOP.Semantics.Membranes.MembraneGraph
import Control.Monad.Views

-- | A membrane-aware monad that tracks the current membrane graph and provides membrane-semantic operations
class (MonadTrans t1, MonadTrans t2, Monad m, Monad (t1 (t2 m))) => MonadMembrane t1 t2 m where
      -- | Gets the identifier of the current membrane.
      currentMembrane :: (t1 (t2 m)) MembraneId
      -- | Gets the current membrane topology.
      getGraph        :: (t1 (t2 m)) MembraneGraph
      -- | Create and add a new membrane to the current membrane topology.
      newMembrane     :: Monad n =>
                          -- | The input programmable layer.
                          (forall t a b. Jp (t1 (t2 m)) a b -> Maybe (Jp (t1 (t2 m)) a b)) ->  
                          -- | The output programmable layer.
                          (forall t a b. Jp (t1 (t2 m)) a b -> Maybe (Jp (t1 (t2 m)) a b)) ->
                          -- | The control flow combinator tag
                          t ->
                          -- | The monad view seen by aspects registered inside the membrane
                          n :><: (t1 (t2 m)) ->                         
                          (t1 (t2 m)) (Membrane m n t)
      -- | Creates a binding between two membranes in the current topology.
      advise          :: (Monad n1, Monad n2) => Membrane m n1 tag1 -> Membrane m n2 tag2 -> (t1 (t2 m)) ()
      -- | Returns whether the membrane of the first argument is bound to the membrane of the second argument in the current topology.
      advises         :: MembraneId -> MembraneId -> (t1 (t2 m)) Bool
      -- | Evaluates a computation inside a given membrane
      evalIn          :: Monad n => Membrane m n t -> (t1 (t2 m)) a -> (t1 (t2 m)) a
      -- | Creates a function that when applied, is evaluated inside the given membrane
      lambda_evalIn   :: (a -> (t1 (t2 m)) b) -> MembraneId -> a -> (t1 (t2 m)) b
