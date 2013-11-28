{-# LANGUAGE TemplateHaskell,
             ScopedTypeVariables,
             FlexibleContexts
  #-}

module AOP.Test.SecurePrivilegedAOT (tests) where

import AOP.Default
import AOP.JPStackT
import AOP.Cflow

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Debug.Trace

tests = testGroup "SecurePrivileged AOT" [test_client1, test_client2]

-------------------------------------------------------------------------------- 
runProg pc aenv c = runIdentity (evalJPStackT (runAOT_sp (SP pc aenv) c) [])

stdTag = $newTag
criticalTag = $newTag

stdComputation = mkFunction (\n -> return (n+32)) stdTag

criticalComputation  = mkFunction (\n -> stdComputation # (n+1)) criticalTag

adv proceed n = return 0

-- Here jpStack is a regular aspect, and since the calls to criticalComputation are protected
-- the pcCflow pointcut does not work correctly because the jp stack does not contain any
-- criticalComputation join points
client1 n = runProg (pcCall criticalComputation) [] (program1 n)
program1 n = do               
              deploy (aspect (pcCflow criticalComputation) adv)
              deploy jpStackAspect
              criticalComputation # n
-- --Tests
-- -- We expect client1 to behave as if there is no pcCflow pointcut.
prop_client1 (n::Integer) = client1 n == runProg pcFalse [] (criticalComputation # n)
test_client1 = testProperty "Client 1" prop_client1


-- Here jpStackAspect is a privileged aspect, so the jp stack is collected correctly, 
-- which allows a correct execution of pcCflow.
client2 n = runProg (pcCall criticalComputation) [EAspect jpStackAspect] (program1 n)
program2 n = do               
              deploy (aspect (pcCflow criticalComputation) adv)
              criticalComputation # n
-- Tests
-- We expect client2 to always return 0, because the aspect should always trigger
prop_client2 n = client2 n == runProg pcFalse [] (return 0)
test_client2 = testProperty "Client 2" prop_client2


