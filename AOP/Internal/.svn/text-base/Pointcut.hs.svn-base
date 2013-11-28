{-# LANGUAGE ExistentialQuantification,
             ScopedTypeVariables,
             RankNTypes
 #-}
module AOP.Internal.Pointcut (
 PC (..),
 RequirePC (..),

 runPC,
) where

import AOP.Internal.Joinpoint
import Data.Typeable
import AOP.Internal.Typeable1Monad

-- | A pointcut is a predicate on the current join point. It is used to identify join points of interest.
data PC m a b = Typeable1Monad m => PC {mpcond :: forall a' b'. m (Jp m a' b' -> m Bool)} 

-- | Extracts the computation resulting of applying a join point to the pointcut
runPC :: Typeable1Monad m => PC m a b -> Jp m a' b' -> m Bool
runPC (PC mpcond) jp = do pccond <- mpcond
                          pccond jp

-- | A RequirePC is not a valid standalone pointcut, it reflects a type requirement and must be combined with a standard PC.
data RequirePC m a b = Typeable1Monad m => RequirePC {mpcond' ::  forall a' b'. m (Jp m a' b' -> m Bool)}

-- | Support for PolyTypeable
instance (Typeable1 m) => Typeable2 (PC m) where
         typeOf2 _ = mkTyConApp (mkTyCon3 "PC" "PC" "PC") 
                     [typeOf1 (undefined :: m ())]

instance (Typeable1 m) => Typeable2 (RequirePC m) where
         typeOf2 _ = mkTyConApp (mkTyCon3 "RequirePC" "RequirePC" "RequirePC") 
                     [typeOf1 (undefined :: m ())]



