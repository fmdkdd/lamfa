{-# LANGUAGE FlexibleContexts,
             ConstraintKinds,
             TypeFamilies,
             MultiParamTypeClasses,
             FlexibleInstances
 #-}


module AOP.Internal.PointcutDef (
 pcCall,
 pcType,
 pcAnd,
 pcOr,
 pcNot,
 pcSeq,
 pcTrue,
 pcFalse,
 pcArgGT
) where

import AOP.Internal.Typeable1Monad
import AOP.Internal.Pointcut
import AOP.Internal.Joinpoint
import AOP.Internal.LessGen
import AOP.Internal.Function
import GHC.Prim(Constraint)
import Unsafe.Coerce
{- |
Built-in pointcuts pcCall and pcType, and pointcut combinators pcAnd, pcOr and pcNot.
Using typeclasses, pointcuts are open for new definitions of functions, like the Function wrapper.
We also define pcSeq, that matches a sequence of two join points.
-}

pcCallPred f t tag = return (\jp -> return (compareFun f tag jp && compareType t jp))

-- | The class of call pointcuts.
class PcCall f where
      pcCall :: (Typeable1Monad m, PolyTypeable (f a b)) => f a b -> PC m a b

-- | Call pointcut for regular functions, which have a shared default function tag. We use StableNames for function equality.
instance PcCall ((->)) where 
         pcCall f = PC (pcCallPred f (polyTypeOf f) defaultFunctionTag)

-- | Call pointcut for our encoding of function identity. Functions that are equal have the same tag.
instance PcCall Function where
         pcCall (Function f tag) = PC (pcCallPred f (polyTypeOf f) tag)

pcTypePred t = (return (\jp -> return (compareType t jp)))

-- | Class for type pointcuts.
class PcType f where
      pcType :: (Typeable1Monad m, PolyTypeable (f a b)) => f a b -> PC m a b

-- | Type pointcut for regular functions.
instance PcType ((->)) where
         pcType f = PC (pcTypePred (polyTypeOf f))

-- | Type pointcut for our encoding of function identity. The function tag has no influence.
instance PcType Function where
         pcType (Function f _) = PC (pcTypePred (polyTypeOf f))

pcCombinator op mpc1 mpc2 =  (do  pc1 <- mpc1
                                  pc2 <- mpc2
                                  return (\jp ->
                                           do
                                            res1 <- pc1 jp
                                            res2 <- pc2 jp
                                            return (res1 `op` res2)))

pcCombinator' op mpc1 =  (do  pc1 <- mpc1
                              return (\jp ->
                                   do
                                      res1 <- pc1 jp
                                      return (op res1)))

-- | And pointcut combinator, overloaded to support PC and RequirePC
class Typeable1Monad m => PCAnd m a1 b1 a2 b2 pct where
  type PCAndCtx m a1 b1 a2 b2 pct :: Constraint
  pcAnd :: PCAndCtx m a1 b1 a2 b2 pct => PC m a1 b1 -> pct m a2 b2 -> PC m a1 b1

-- | When combining two PC pointcuts, the matched types t1 and t2 must be the same
-- | This expressed in the constraint t1 ~ t2
-- (Note: if we use only t instead of t1 t2 and t1 ~ t2, we get a compile error in AOTTests.hs)
instance Typeable1Monad m => PCAnd m a1 b1 a2 b2 PC where
         type PCAndCtx m a1 b1 a2 b2 PC = ((a1 -> b1) ~ (a2 -> b2))
         pcAnd (PC mpc1) (PC mpc2) = PC (pcCombinator (&&) mpc1 mpc2)

-- | When combining a PC with a RequirePC we constraint t1 to be LessGen than t2
instance Typeable1Monad m => PCAnd m a1 b1 a2 b2 RequirePC where
         type PCAndCtx m a1 b1 a2 b2 RequirePC = (LessGen (a1 -> b1) (a2 -> b2))
         pcAnd (PC mpc1) (RequirePC mpc2) = PC (pcCombinator (&&) mpc1 mpc2)

-- | Or pointcut combinator.
pcOr :: (Typeable1Monad m, LeastGen (a1 -> b1) (a2 -> b2) (agen -> bgen)) => PC m a1 b1 -> PC m a2 b2 -> PC m agen bgen
pcOr (PC mpc1) (PC mpc2) = PC (pcCombinator (||) mpc1 mpc2)

-- | Not pointcut combinator.
pcNot :: (Typeable1Monad m) => PC m a1 b1 -> PC m a2 b2 
pcNot (PC mpc) =  PC (pcCombinator' not mpc)

-- | Matches the sequence of two pcs
pcSeq :: (Typeable1 m, MonadState Bool m) => PC m a1 b1 -> PC m a2 b2 -> PC m a2 b2
pcSeq (PC mpc1) (PC mpc2) = 
       PC (do  pc1 <- mpc1
               pc2 <- mpc2
               return (\ jp -> do b <- get
                                  if b 
                                    then pc2 jp
                                    else do pc1_ <- pc1 jp
                                            put pc1_
                                            return False))

-- | True pointcut.
pcTrue :: (Typeable1Monad m) => PC m a b
pcTrue =  PC (return (\jp -> return True))

-- | False pointcut.
pcFalse :: (Typeable1Monad m) => PC m a b
pcFalse =  PC (return (\jp -> return False))

-- User-Defined Pointcuts

pcArgGT :: (Typeable1Monad m, Ord a) => a -> RequirePC m a b 
pcArgGT n = RequirePC $ return (\jp -> return (unsafeCoerce (getJpArg jp) >= n))
