{-# LANGUAGE ExistentialQuantification,             
             FlexibleContexts,
             TypeSynonymInstances
 #-}

module AOP.Internal.Aspect ( 
 Aspect (..),
 EAspect (..),
 AspectEnv,
 AspectHandle,
 deleteAsp
) where

import AOP.Internal.Pointcut
import AOP.Internal.Advice
import AOP.Internal.PolyTypeable
import AOP.Internal.LessGen
import Data.Unique
import AOP.Internal.Typeable1Monad

type AspectHandle = Unique

-- Typed first-class aspect
-- An aspect is tagged with a Unique value, used for identity
data Aspect m a b c d = LessGen (a -> b) (c -> m d) => 
                        Aspect (PC m a b) (Advice m c d) AspectHandle 

-- Aspect with hidden types, to be used in the aspect environment
data EAspect m = forall a b c d. LessGen (a -> b) (c -> m d) => EAspect (Aspect m a b c d)

-- Aspect environment
type AspectEnv m = [EAspect m]

instance Show AspectHandle where
         show handle = show $ hashUnique handle

instance Show (Aspect m a b c d) where
         show (Aspect pc adv handle) = show handle

instance Show (EAspect m) where
         show (EAspect (Aspect pc adv handle)) = show handle

-- Deletes asp from the aspect environment, used in undeploy
deleteAsp :: Typeable1Monad m => EAspect m -> AspectEnv m -> AspectEnv m
deleteAsp asp = filter (\asp' -> asp /= asp')

-- Support por PolyTypeable
instance PolyTypeable Unique where
         polyTypeOf _ = mkTyConApp (mkTyCon3 "GHC" "Unique" "") []

-- Notion of aspect equality to delete aspects from the aspect environment
instance Typeable1Monad m => Eq (EAspect m) where
         EAspect (Aspect _ _ u1) == EAspect (Aspect _ _ u2) = u1 == u2

