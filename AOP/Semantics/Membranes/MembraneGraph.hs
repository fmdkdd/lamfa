{-# LANGUAGE FlexibleInstances,
             Rank2Types,
             ExistentialQuantification,
             KindSignatures,
             TypeOperators,
             GeneralizedNewtypeDeriving
  #-}

module AOP.Semantics.Membranes.MembraneGraph where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot
import AOP.Internal.Joinpoint
import AOP.Internal.AOTType
import Control.Monad.Views
import Control.Monad.State
import Control.Monad.Cont

-- | Identifier of a membrane in the topology
type MembraneId = Node

-- | The MembraneT transformer is a state-like monad transformer
newtype MembraneT m a = MembraneT { unMembraneT :: StateT (MembraneId, MembraneGraph) m a }
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)

runMG = runStateT . unMembraneT
mkMembraneT = MembraneT . StateT

-- | A programmable membrane.
data Membrane m n t = (Monad m, Monad n) => 
                  Membrane { -- | The node identifier in the topology.
                             mbId  :: MembraneId, 
                             -- | The programmable layer for receiving join points.
                             jpIn  :: forall a b. Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b),
                             -- | The programmable layer for emitting join points.
                             jpOut :: forall a b. Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b),
                             -- | Tag for control flow combinator required in aspects.
                             --   The valid options are the empty data constructors:
                             --   CombinationAdvice, ReplacementAdvice, AugmentationAdvice, NarrowingAdvice
                             mbTag   :: t,
                             -- | The monad view seen by aspects registered into the membrane     
                             mbView  :: n :><: AOT (MembraneT m)
                           }

-- | The membrane topology is a directed graph with existentially quantified membranes.
data EMembrane = forall m n t. (Monad m, Monad n) => EMembrane (Membrane m n t)
type MembraneGraph = Gr (EMembrane) ()

-- | Creates an empty membrane topology
emptyMembraneGraph :: MembraneGraph
emptyMembraneGraph = empty

-- | Creates a membrane with a node identifier that is unique with respect to the given topology.
makeMembrane :: (Monad m, Monad n) =>
                -- | The topology.
                MembraneGraph -> 
                -- | The input programmable layer.
                (forall a b. Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b)) ->
                -- | The output programmable layer.
                (forall a b. Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b)) ->
                -- | The control flow combinator tag
                t ->
                -- | The monad view seen by aspects registered into the membrane
                n :><: AOT (MembraneT m) ->
                Membrane m n t
makeMembrane graph jpin jpout comb view = let mbId = head $ newNodes 1 graph
                                    in  Membrane mbId jpin jpout comb view

-- | Adds a membrane to a topology with no bindings.
addMembrane :: (Monad m, Monad n) => Membrane m n t -> MembraneGraph -> MembraneGraph
addMembrane membrane graph = insNode (mbId membrane, EMembrane membrane) graph

-- | Binds to membranes, identified by their id, in the given topology
bindMembranes :: (MembraneId, MembraneId) -> MembraneGraph -> MembraneGraph
bindMembranes (mbId1, mbId2) graph = insEdge (mbId1, mbId2, ()) graph

instance Show (Membrane m n t) where
         show (Membrane id _  _ _ _) = "Mb(" ++ show id ++ ")"

instance Show EMembrane where
         show (EMembrane m) = show m

instance Graph gr => Show (gr EMembrane ()) where
        show gr = showDot (fglToDot gr)