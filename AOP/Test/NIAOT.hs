{-# LANGUAGE TemplateHaskell,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             RankNTypes,
             FlexibleContexts,
             FlexibleInstances,
             ExistentialQuantification,
             IncoherentInstances
 #-}

module AOP.Test.NIAOT (tests) where

import AOP.Default
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

tests = testGroup "NIAOT" [test_programNI, test_programNI2]

--------------------------------------------------------------------------------


-- Monadic stack (and run function) for non-interfering state
type NIAOState = NIAOT (StateT Bool) ((StateT String (StateT Int Identity)))

runNIAOState c nbase nadv = runIdentity (evalStateT (evalStateT (evalStateT (runNIAOT c) nadv) " ") nbase)
                                                 

-- A tagged caching function
-- it increases the current cache by m+1
cachingSucc :: Function Int (NIAOState Int)
cachingSucc = mkFunction (\ m -> do n <- (niLift . lift . lift) $ get
                                    (niLift . lift . lift . put) (n+m+1)
                                    return (n+m)) cachTag

-- Monadic successor function
successor :: Function Int (NIAOState Int)
successor = Function (\ n -> return (n+1)) succTag

pureAdv proceed n = proceed (n+10)

-- Fresh tags for the cachingSucc and succTag functions
-- To be used in our notion of equality based.
cachTag = $newTag
succTag = $newTag

-- initializing the Int and Bool states on the monadic stack
-- the integer state is only available to aspects, and the boolean state only to base code
clientNI c = runNIAOState c 1 False

-- niAspect enforces a non-interfering pointcut and a non-interfering advice
niAsp :: Aspect NIAOState Int (NIAOState Int) Int Int
niAsp = niAspect (pcCall successor `pcSeq` pcCall cachingSucc) pureAdv

-- the pointcut sequence triggers execution of pureAdv
-- the cachingSucc correctly updates the state without interfering with the base state
programNI :: Int -> NIAOState Int
programNI s = do deploy niAsp 
                 cachingSucc # 1
                 successor # 1
                 cachingSucc # s

prop_programNI s = clientNI (programNI s) == 13+s
test_programNI = testProperty "Program NI" prop_programNI

programNI2 :: Int -> NIBase (StateT String (StateT Int Identity)) Int Int -> NIAOState Int
programNI2 s f = niBase f s

niFoo :: Monad m => NIBase m Int Int
niFoo n = return (n+100)

prop_programNI2 s = clientNI (programNI2 s niFoo) == 100+s
test_programNI2 = testProperty "Program NI 2" prop_programNI2
