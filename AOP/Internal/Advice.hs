{-# LANGUAGE FlexibleContexts,
             ExistentialQuantification,
             RankNTypes,
             TypeFamilies,
             MultiParamTypeClasses,
             TypeOperators,
             FlexibleInstances
 #-}

module AOP.Internal.Advice (
 Advice,
 -- AdviceCombinator (..),
 CombinationAdvTag (..),
 ReplacementAdvTag (..),
 AugmentationAdvTag (..),
 NarrowingAdvTag (..),
 -- RestrictedAdv (..),
 CombinationAdv (..),
 NarrowingAdv (..),
 ReplacementAdv (..),
 AugmentationAdv (..),
 -- withView,
 apply_adv,
 replace,
 augment,
 narrow, 
 before,
 beforeAdv,
 after,
 Narrow,
 Augment,
 Replace,
) where

import Unsafe.Coerce
-- import Control.Monad.Views
import GHC.Prim
-- import AOP.Internal.WithView FIX mutual recursion

type Advice m a b = (a -> m b) -> a -> m b

{- The use of unsafeCoerce below is safe because any function
   that a pc matches is compatible by construction with the advice.
   See Section 4 of the paper.
-}

-- | Coerces t2 to be compatible with the advice. It passes t1 as a the proceed argument of the advice.
-- This coercion is safe, as described in Section 4 of the paper.
apply_adv :: Advice m a b -> t2 -> t2
apply_adv adv f = unsafeCoerce adv f


-- Enforcement of control flow properties, adapted from EffectiveAdvice

-- Replacement: no calls to proceed. Computation is replaced by radv.
type Replace m a b = (a -> m b)
replace :: Replace m a b -> Advice m a b
replace radv proceed = radv

-- Augmentation: proceed is called only once. Behavior can be specified before and after
-- the implicit call to proceed. The second part has access to the return value,
-- and to the value returned by the first part.
type Augment a b c m = (a -> m c, a -> b -> c -> m ())
augment :: Monad m => Augment a b c m -> Advice m a b
augment (before, after) proceed arg = do c <- before arg
                                         b <- proceed arg
                                         after arg b c
                                         return b

-- before/after advice can be expressed as particular cases of augmentation
before before = augment (\a -> before a >> return (), \a b c -> return ())
after  after  = augment (\_ -> return (), \a b c -> after a b)

beforeAdv before = (\a -> before a >> return (), \a b c -> return ())

-- Narrowing: proceed is called at most once. A runtime choice can be
-- made between replacement or augmentation advice. Typical case: memoization
type Narrow a b c m = (a -> m Bool, (a -> m c, a -> b -> c -> m ()), Replace m a b)
narrow :: Monad m => Narrow a b c m -> Advice m a b
narrow (p, aug, rep) proceed arg = 
       do test <- p arg
          if test
             then augment aug proceed arg
             else replace rep proceed arg

---------------------------------------------------------------------------------
data CombinationAdvTag  (m :: * -> *) = CombinationAdvTag 
data ReplacementAdvTag  (m :: * -> *) = ReplacementAdvTag
data AugmentationAdvTag (m :: * -> *) = AugmentationAdvTag
data NarrowingAdvTag    (m :: * -> *) = NarrowingAdvTag

newtype CombinationAdv m a b = CombinationAdv ((a -> m b) -> a -> m b)
combination (CombinationAdv adv) = adv

newtype ReplacementAdv m a b = ReplacementAdv { rAdv :: Replace m a b }
replacement (ReplacementAdv rAdv) = replace rAdv

newtype AugmentationAdv m a b = AugmentationAdv { augAdv :: Augment a b () m }
augmentation (AugmentationAdv augAdv) = augment augAdv

newtype NarrowingAdv m a b = NarrowingAdv { nAdv :: Narrow a b () m }
narrowing (NarrowingAdv nAdv) = narrow nAdv

-- withView :: (Monad n, Monad m) => n :><: m -> Advice n a b -> Advice m a b
-- withView v adv proceed arg = from v $ adv (\ a -> to v (proceed a)) arg

-- data RestrictedAdv n m c = RestrictedAdv (n :><: m) c

-- ---------------------------------------------------------------------------------
-- class (Monad (CombinatorMonad c)) => AdviceCombinator c where
--   type CombinatorType c a b
--   type CombinatorMonad c :: * -> *
--   toAdvice :: c -> CombinatorType c a b -> Advice (CombinatorMonad c) a b

-- instance Monad m => AdviceCombinator (CombinationAdvTag m) where
--   type CombinatorType (CombinationAdvTag m) a b = CombinationAdv m a b
--   type CombinatorMonad (CombinationAdvTag m) = m
--   toAdvice CombinationAdvTag = combination

-- instance Monad m => AdviceCombinator (ReplacementAdvTag m) where
--   type CombinatorType (ReplacementAdvTag m) a b = ReplacementAdv m a b
--   type CombinatorMonad (ReplacementAdvTag m) = m
--   toAdvice ReplacementAdvTag = replacement

-- instance Monad m => AdviceCombinator (AugmentationAdvTag m) where
--   type CombinatorType (AugmentationAdvTag m) a b = AugmentationAdv m a b
--   type CombinatorMonad (AugmentationAdvTag m) = m
--   toAdvice AugmentationAdvTag = augmentation

-- instance Monad m => AdviceCombinator (NarrowingAdvTag m) where
--   type CombinatorType (NarrowingAdvTag m) a b = NarrowingAdv m a b
--   type CombinatorMonad (NarrowingAdvTag m) = m
--   toAdvice NarrowingAdvTag = narrowing

-- instance (Monad m, Monad n, AdviceCombinator c, CombinatorMonad c ~ n) =>
--          AdviceCombinator (RestrictedAdv n m c) where
--   type CombinatorType (RestrictedAdv n m c) a b = CombinatorType c a b
--   type CombinatorMonad (RestrictedAdv n m c) = m
--   toAdvice (RestrictedAdv v c) = withView v . toAdvice c

