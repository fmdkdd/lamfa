{-# LANGUAGE FlexibleContexts,
             ExistentialQuantification              
 #-}

module AOP.Internal.Joinpoint (
 Jp(..),
 newjp,
 compareFun,
 compareType,
 getJpArg
) where

import AOP.Internal.StableNamesEq
import AOP.Internal.Typeable1Monad
import AOP.Internal.PolyTypeableUtils
import AOP.Internal.Function

-- | Join points are function applications. We store the function and the argument.
-- | We add a FunctionTag value to use for function equality (see module Function)
data Jp m a b = (Monad m, PolyTypeable (a -> m b)) => Jp (a -> m b) FunctionTag a

-- | Creates a join point with given function, tag, and argument
newjp :: (Monad m, PolyTypeable (a -> m b)) => (a -> m b) -> FunctionTag -> a -> Jp m a b
newjp f t a = Jp f t a

-- | Comparing identity of functions:
-- | When given a join point with a regular function (signaled by the default tag)
-- | then we use StableNames for comparison. If the functions are wrapped,
-- | we compare the tags
compareFun :: Monad m => t -> FunctionTag -> Jp m a b -> Bool
compareFun f ft (Jp g t _) = if t == defaultFunctionTag
                                then stableNamesEq f g
                                else ft == t

-- | Compare types to see if type representation t is less general 
-- | than the type of the function associated to the join point
compareType :: (Monad m, PolyTypeable (a -> m b)) => TypeRep -> Jp m a b -> Bool
compareType t (Jp f _ _) = isLessGeneral t (polyTypeOf f)

-- | Gets the argument bound to the join point
getJpArg :: Monad m => Jp m a b -> a
getJpArg (Jp _ _ x) = x
