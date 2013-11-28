{-# LANGUAGE ExistentialQuantification,
             FlexibleContexts,
             DeriveDataTypeable,
             RankNTypes    
 #-}

module AOP.Internal.AspectDef ( 
 aspect,
 purePcAspect,
 pureAdviceAspect,
 pureAspect,
 newAspectHandle
) where

import AOP.Internal.Pointcut
import AOP.Internal.Advice
import AOP.Internal.LessGen
import AOP.Internal.Aspect
import AOP.Internal.Typeable1Monad
import Data.Unique
import System.IO.Unsafe

-- | Creates a new AspectHandle, i.e. a unique aspect identifier. 
newAspectHandle :: AspectHandle
newAspectHandle = unsafePerformIO newUnique

aspect :: (Monad m, LessGen (a1 -> b1) (a2 -> m b2)) => 
          PC m a1 b1 -> Advice m a2 b2 -> Aspect m a1 b1 a2 b2
aspect pc adv = Aspect pc adv newAspectHandle

-- | Pure PC/Advice. Purity is obtained by parameterizing the definitions on every monad m.
type PurePC a b = forall m. Typeable1Monad m => PC m a b 
type PureAdvice a b = forall m. Typeable1Monad m => Advice m a b 

-- | Builds an aspect with a pure pointcut
purePcAspect :: (Typeable1Monad m, LessGen (a -> b) (c -> m d)) => 
                PurePC a b -> Advice m c d -> Aspect m a b c d
purePcAspect pc adv = aspect pc adv

-- | Builds an aspect with a pure advice
pureAdviceAspect :: (Typeable1Monad m, LessGen (a -> b) (c -> m d)) => 
                    PC m a b -> PureAdvice c d -> Aspect m a b c d
pureAdviceAspect pc adv = aspect pc adv

-- | Builds an aspect with pure pointcut and advice
pureAspect :: (Typeable1Monad m, LessGen (a -> b) (c -> m d)) => 
              PurePC a b -> PureAdvice c d -> Aspect m a b c d
pureAspect pc adv = aspect pc adv