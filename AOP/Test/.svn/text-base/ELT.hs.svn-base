{-# LANGUAGE TemplateHaskell,
             FlexibleContexts
  #-}

module AOP.Test.ELT (tests) where

import AOP.ExecutionLevels
import Debug.Trace

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = testGroup "Execution Levels Semantics" [testELT1]

-- Tests
testELT1 = testCase "Case 1"  $ (runProgEL program) @?= "no loop"

--------------------------------------------------------------------------------
  
type M = AOT (ELT Identity)

runProgEL c = runIdentity (runELT (runAOT c) 0)

showTag = $newTag

showM ::  Show a => Function a (M String)
showM = mkFunction (\a -> return (show a)) showTag

logAdv :: (Show a, Show b) => Advice M a b
logAdv proceed a = do argStr <- showM # a
                      result <- proceed a
                      resStr <- showM # result
                      trace ("Arg: " ++ argStr ++ " Result: " ++ resStr)
                            (return result)

-- In the default semantics of AOP this programs loops, because logAdv is triggered infinitely
-- many times when it evaluates showM # result. However, with the semantics of execution levels
-- the program executes correctly.
program = do deploy logAspect
             showM # (1::Int)
             return "no loop"             
 where logAspect = aspect (pcCall (showM :: Function Int (M String))) logAdv




