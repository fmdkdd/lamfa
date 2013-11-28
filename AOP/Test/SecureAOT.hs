{-# LANGUAGE ScopedTypeVariables
  #-}

-- module AOP.Test.SecureAOT where --(tests, client1) where

import AOP.Default
import AOP.JPStackT
import AOP.Cflow
import AOP.Test.CriticalComputation

import Test.Framework (testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

main = defaultMain [tests]

tests = testGroup "SecureAOT" [test_client1]

-------------------------------------------------------------------------------- 
{- This example tests that (aspect (pcCall stdComputation) adv) does not
   interfere with evaluation of criticalComputation, because the internal join points
   are protected. So the expected result is equal to that of just calling criticalComputation.
   criticalComputation and runSafe are imported from CriticalComputation.hs
-}

runProg c = runIdentity (runSafe c)

adv proceed n = return 0

client1 n = runProg (program1 n)
program1 n = do deploy (aspect (pcCall stdComputation) adv)
                deploy jpStackAspect
                criticalComputation # n
--Tests
prop_client1 (n::Integer) = client1 n == runProg (criticalComputation # n)
test_client1 = testProperty "Client 1" prop_client1

runProg2 c = runIdentity (evalJPStackT (runAOT_s (EPC pcFalse) c) [])

-- this is rejected because runProg2 expects something of 
-- type :: AOT_s (JpStackT Identity) a
--client2 n = runProg2 (program1 n) 