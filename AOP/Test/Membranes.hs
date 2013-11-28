{-# LANGUAGE Rank2Types,
             TemplateHaskell,
             FlexibleContexts,
             KindSignatures,
             ScopedTypeVariables
  #-}

module AOP.Test.Membranes (tests) where

import AOP.Membranes
import Data.List
import Debug.Trace
import Unsafe.Coerce

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

tests = testGroup "Membrane Semantics" [
                  testTowerExample,
                  testTreeExample,
                  testGraphExample,
                  testOpaqueExample,
                  testTranslucentExample1,
                  testTranslucentExample2,
                  testTranslucentExample3
        ]

-- ============================ MAScheme examples ============================
-- Adapted from Taming Aspects with Membranes, FOAL 2012.

-- Basic utility functions

accessDiskTag = $newTag
accessDisk :: (MonadWriter String m) => Function () (m ())
accessDisk = mkFunction (\ _ -> tell "Disk") accessDiskTag

getUrlTag = $newTag
getUrl :: (MonadWriter String m) => Function String (m ())
getUrl = mkFunction (\ s -> tell "Url") getUrlTag

checkCacheTag = $newTag
checkCache :: (Typeable1Monad m, OpenApp Function m, MonadWriter String m) => Function () (m ())
checkCache = mkFunction (\ _ -> accessDisk # ()) checkCacheTag

runBrowserTag = $newTag
runBrowser :: (Typeable1Monad m, OpenApp Function m, MonadWriter String m) => Function () (m ())
runBrowser = mkFunction (\ _ -> do getUrl # "http://test.url"; accessDisk # ()) runBrowserTag

cacheAdvice :: (Typeable1Monad m, OpenApp Function m, MonadWriter String m) => Advice m a b
cacheAdvice proceed arg = do tell "Cache"; checkCache # (); proceed arg

quotaAdvice :: (Typeable1Monad m, OpenApp Function m, MonadWriter String m) => Advice m () ()
quotaAdvice proceed arg = do tell "Quota"; proceed arg

type MS = AOT (MembraneT (StateT Int (StateT Bool (WriterT String Identity))))
runMS c = runIdentity $ runWriterT $ evalStateT (evalStateT (runMembraneT (runAOT c) (0, emptyMembraneGraph)) 0) True

-- Tower topology, i.e. execution levels
towerExample :: MS Int
towerExample = do l0 <- newMembrane Just Just CombinationAdvice idv
                  l1 <- newMembrane Just Just CombinationAdvice idv
                  l2 <- newMembrane Just Just CombinationAdvice idv
                  advise l1 l0
                  advise l2 l1
                  register (pcCall getUrl) (Combination cacheAdvice) l1
                  register (pcCall accessDisk) (Combination quotaAdvice) l2
                  evalIn l0 (runBrowser # ())
                  sBool <- (lift $ lift $ lift $ get) 
                  sInt <- get
                  trace ("State Bool: " ++ show sBool ++ " State Int: " ++ show sInt) return ()
                  return 0

testTowerExample = testCase "Tower Example" $ (runMS towerExample) @?= (0, "CacheQuotaDiskUrlDisk")


-- Tree topology example
treeExample :: MS Int
treeExample = do l0 <- newMembrane Just Just CombinationAdvice idv
                 l1 <- newMembrane Just Just CombinationAdvice idv
                 l2 <- newMembrane Just Just CombinationAdvice idv
                 l3 <- newMembrane Just Just CombinationAdvice idv
                 advise l1 l0
                 advise l2 l1
                 advise l3 l0
                 register (pcCall getUrl)     (Combination cacheAdvice) l1
                 register (pcCall accessDisk) (Combination quotaAdvice) l2
                 register (pcCall accessDisk) (Combination quotaAdvice) l3
                 evalIn l0 (runBrowser # ())
                 return 0

testTreeExample = testCase "Tree Example" $ (runMS treeExample) @?= (0, "CacheQuotaDiskUrlQuotaDisk")


-- Graph topology example
graphExample :: MS Int
graphExample = do l0 <- newMembrane Just Just CombinationAdvice idv
                  l1 <- newMembrane Just Just CombinationAdvice idv
                  l2 <- newMembrane Just Just CombinationAdvice idv
                  advise l1 l0
                  advise l2 l0
                  advise l2 l1
                  register (pcCall getUrl)     (Combination cacheAdvice) l1
                  register (pcCall accessDisk) (Combination quotaAdvice) l2
                  evalIn l0 (runBrowser # ())
                  return 0

testGraphExample = testCase "Graph Example" $ (runMS graphExample) @?= (0, "CacheQuotaDiskUrlQuotaDisk")


-- Opaque membrane example
opaqueExample :: MS Int
opaqueExample = do l0 <- newMembrane Just (\ jp -> Nothing) CombinationAdvice idv
                   l1 <- newMembrane Just Just CombinationAdvice idv
                   advise l1 l0
                   register (pcCall getUrl) (Combination cacheAdvice) l1
                   evalIn l0 (runBrowser # ())
                   return 0

testOpaqueExample = testCase "Opaque Example" $ (runMS opaqueExample) @?= (0, "UrlDisk")


traceAdvice :: (MonadWriter String m) => Advice m a b
traceAdvice proceed arg = do tell "Trace"
                             proceed arg

-- Translucent membrane example
-- Don't expose calls to getUrl
translucentExample1 :: MS Int
translucentExample1 = do l0 <- newMembrane Just (\ jp@(Jp f t _) -> if (t == getUrlTag)
                                                                      then Nothing
                                                                      else Just jp)
                                           CombinationAdvice idv
                         l1 <- newMembrane Just Just CombinationAdvice idv
                         advise l1 l0
                         register (pcOr (pcCall getUrl) (pcCall accessDisk)) (Combination traceAdvice) l1
                         evalIn l0 (runBrowser # ())
                         return 0

testTranslucentExample1 = testCase "Translucent Example 1" $
                          (runMS translucentExample1) @?= (0, "UrlTraceDisk")

-- | Only expose calls to getUrl for non-https requests
--   Somehow argument coercing must be done based on the join point, and the pointcut that accepted it.
--   So one way would be to emulate the behavior of pcAnd / pcArg
translucentExample2 :: MS Int
translucentExample2 = do l0 <- newMembrane Just (\ jp@(Jp f t arg) ->
                                                   if (t == getUrlTag && (isPrefixOf "http://" (unsafeCoerce arg))) -- ocurrence typing??
                                                      then (Just jp)
                                                      else Nothing)
                                           CombinationAdvice idv
                         l1 <- newMembrane Just Just CombinationAdvice idv
                         advise l1 l0
                         register (pcOr (pcCall getUrl) (pcCall accessDisk)) (Combination traceAdvice) l1    
                         evalIn l0 (runBrowser # ())
                         return 0

testTranslucentExample2 = testCase "Translucent Example 2" $
                          (runMS translucentExample2) @?= (0, "TraceUrlDisk")


-- | Only expose calls to getUrl for https requests
--   Same observations as in 'translucentExample2'.
translucentExample3 :: MS Int
translucentExample3 = do l0 <- newMembrane Just (\ jp@(Jp f t arg) ->
                                                   if (t == getUrlTag && (isPrefixOf "https://" (unsafeCoerce arg)))
                                                      then Just jp
                                                      else Nothing)
                                           CombinationAdvice idv
                         l1 <- newMembrane Just Just CombinationAdvice idv
                         advise l1 l0
                         register (pcOr (pcCall getUrl) (pcCall accessDisk)) (Combination traceAdvice) l1    
                         evalIn l0 (runBrowser # ())
                         return 0

testTranslucentExample3 = testCase "Translucent Example 3" $
                          (runMS translucentExample3) @?= (0, "UrlDisk")
