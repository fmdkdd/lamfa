{-# LANGUAGE TemplateHaskell,
             FlexibleContexts

 #-}

module AOP.Test.Macros (tests) where

import AOP.Default
import Debug.Trace

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- These tests don't work using ghci
-- They must be compiled with the -O flag for optimizations.
-- Use/edit runTests.sh to compile and run these tests.

tests = testGroup "Macros" [test_thclient, test_thclient2, test_thclient3]

--------------------------------------------------------------------------------

{- Advising a function of a type class:

deployTC takes as argument the name of a type class, and the name of a function
of that type class. Because of compiler inlining, there is a type error if the
names do not really correspond to the required elements.

The macro uses all the instances currently in scope and deploys an aspect
for each one of them. In GHCi you can see the translation by setting the
--dump-splices flag. The command is :set --ddump-splices.

The [| ... |] expression is a quotation, to access the AST corresponding to 
the internal expression. We quote the advice expression to generate the
annotated deployments.

-}

-- Some preamble
-- Type of programs and runner function
type AOID = AOT Identity
runAOID :: AOID a -> a
runAOID c = runIdentity (runAOT c)


-- We define a dummy type class with few instances to exemplify the translation
class Bar a where
      doBar :: a -> AOID Int

instance Bar Int where
      doBar i = return (i+1)

instance Bar Float where
      doBar i = return 0

advice :: (Bar a, Num a) => Advice AOID a Int
advice proceed n = proceed (n+100)

thclient n x = runAOID (thprogram n x)
thprogram :: Int -> Float -> AOID Int
thprogram n x = do
                  $(deployTC "Bar" "doBar" [| advice |])
                  z <- doBar # n
                  y <- doBar # x
                  return (z+y)
-- Tests
prop_thclient n x = thclient n x == (n+100+1)
test_thclient = testProperty "TH Client" prop_thclient


{- Advising a bounded polymorphic function with a bounded polymorphic advice.

We obtain all the instances that solve the required constraints and then deploy only on those instances. The aspect can use either a pcCall or a pcType pointcut. This could be extended to support arbitrary pointcuts
-}
class Foo a
instance Foo Int

-- We define a monadic stack with support for logging using the Writer monad
type AOLog = AOT (WriterT String Identity)
runAOLog :: AOLog a -> (a, String)
runAOLog c = runIdentity (runWriterT (runAOT c))


-- Using stable names the call poincut does not match, so we use tagged functions

polyHeadTag = $newTag

polyHead :: (Show a, Foo a) => Function [a] (AOLog a)
polyHead = mkFunction (\xs -> return (head xs)) polyHeadTag

polyAdvice :: (Typeable1Monad m, MonadWriter String m, Show a) => Advice m [a] a
polyAdvice proceed arg = do tell "Advice called\n"
                            proceed arg

-- Using a pcCall pointcut
thclient2 = runAOLog thprogram2
thprogram2 = do
               $(deployCall "polyHead" [| polyAdvice |])
               x <- polyHead # ([1, 2, 3] :: [Int])
               return (x, 'a')

test_thclient2 = testCase "Deploy Call" $ ((1, 'a'), "Advice called\n") @?= thclient2

-- Using a pcType pointcut
thclient3 = runAOLog thprogram3
thprogram3 = do
                 $(deployType "polyHead" [| polyAdvice |])
                 x <- polyHead # ([1, 2, 3] :: [Int])
                 return (x, 'a')

test_thclient3 = testCase "Deploy Type" $ ((1, 'a'), "Advice called\n") @?= thclient3

