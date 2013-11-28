{-# LANGUAGE FlexibleContexts,
             RankNTypes
 #-}

module AOP.Internal.NonInterferenceDef ( 
 niPcAspect,
 niAdviceAspect,
 niAspect,
 niBase,
 niAdvice,
 NIPC,
 NIBase,
 NIAdvice
) where

import AOP.Internal.Pointcut
import AOP.Internal.Advice
import AOP.Internal.LessGen
import AOP.Internal.Aspect
import AOP.Internal.NIAOT
import AOP.Internal.Typeable1Monad
import AOP.Internal.AspectDef

-- | Types of Non-Interfering poincut, advice, and base code

-- | The non-interference property is obtained by parameterizing on the monad m, or the monad transformer t
type NIPC t a b = forall m. (Typeable1Monad m, Typeable1Monad (t m)) => PC (NIAOT t m) a b 

-- | Non-interfering advice. The advice must work uniformly on any monad underlying monad m
type NIAdvice t a b = forall m. (Typeable1Monad m, Typeable1Monad (t m)) => Advice (NIAOT t m) a b 

-- | Non-interfering base computation. The program must work uniformly on any transformer t
type NIBase m a b = forall t. (MonadTrans t, Typeable1Monad (t m)) => a -> NIAOT t m b

-- | Builds an aspect with a NI pointcut
niPcAspect ::
   (Typeable1Monad m, Typeable1Monad m', Typeable1Monad (t m),
    LessGen (a -> m' b) (c -> NIAOT t m d)) => 
    NIPC t a (m' b) -> Advice (NIAOT t m) c d -> 
    Aspect (NIAOT t m) a (m' b) c d
niPcAspect pc adv = aspect pc adv

-- | Builds an aspect with a NI advice
niAdviceAspect ::
  (Typeable1Monad m, Typeable1Monad m', Typeable1Monad (t m),
   LessGen (a -> m' b) (c -> NIAOT t m d)) => 
   PC (NIAOT t m) a (m' b) -> NIAdvice t c d ->
   Aspect (NIAOT t m) a (m' b) c d
niAdviceAspect pc adv = aspect pc adv

-- | Builds a NI aspect, that is it has NI pointcut and advice
niAspect ::
  (Typeable1Monad m, Typeable1Monad m', Typeable1Monad (t m),
   LessGen (a -> m' b) (c -> NIAOT t m d)) => 
   NIPC t a (m' b) -> NIAdvice t c d ->  Aspect (NIAOT t m) a (m' b) c d
niAspect pc adv = aspect pc adv

-- | Builds a NI base program
niBase :: (MonadTrans t, Typeable1Monad (t m)) => NIBase m a b -> a -> NIAOT t m b 
niBase f = f

-- | Accepts an NIAdvice only
niAdvice :: (Typeable1Monad m, Typeable1Monad (t m)) =>
            NIAdvice t a b -> Advice (NIAOT t m) a b
niAdvice adv = adv
