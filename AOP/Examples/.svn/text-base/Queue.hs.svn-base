{-# LANGUAGE FlexibleContexts,
             NoMonomorphismRestriction,
             TemplateHaskell
  #-}

module AOP.Examples.Queue where

import AOP.Default
import Debug.Trace

-- =======================================================================
-- Queue example using a concrete monadic stack M

-- Defining getStateT and putStateT
-- Note: these definitions correspond to the MonadState instance defined
-- by StateT. We show them here for illustration purposes, as we also
-- do in the paper.

getStateT :: Monad m => StateT s m s
getStateT = StateT $ \s -> return (s, s)

putStateT :: Monad m => s -> StateT s m ()
putStateT s = StateT $ \ _ -> return ((), s)

type M = StateT [Int] Identity

runM :: M a -> a
runM c = runIdentity $ evalStateT c []

enqueue :: Int -> M ()
enqueue n = do queue <- getStateT
               putStateT $ queue ++ [n]

dequeue :: M Int
dequeue = do queue <- getStateT
             putStateT $ tail queue
             return $ head queue

-- Defining throwErrorT and catchErrorT.
-- Note: these definitions correspond to the MonadError instance defined by
-- ErrorT. 

throwErrorT :: Monad m => e -> ErrorT e m a
throwErrorT l = ErrorT $ return (Left l)

catchErrorT :: Monad m => ErrorT e m a -> (e -> ErrorT e m a) -> ErrorT e m a
m `catchErrorT` h = ErrorT $ do
                    a <- runErrorT m
                    case a of
                         Left  l -> runErrorT (h l)
                         Right r -> return (Right r)
               
-- =======================================================================
-- Combining State and Error-Handling Effects

type M' = StateT [Int] (ErrorT String Identity)

runM' :: M' a -> Either String a
runM' c = runIdentity $ runErrorT $ evalStateT c []

enqueue' :: Int -> M' ()
enqueue' n = do queue <- getStateT
                putStateT $ queue ++ [n]

dequeue' :: M' Int
dequeue' = do queue <- getStateT
              if null queue
                 then (lift . throwErrorT) "Queue is empty" -- fails if we don't lift
                 else do putStateT $ tail queue
                         return $ head queue

-- =======================================================================
-- Using Polymorphism on the Monadic Stack

enqueue'' :: (Monad m, MonadState [Int] m) => Int -> m ()
enqueue'' n = do queue <- get
                 put $ queue ++ [n]

dequeue'' :: (Monad m, MonadState [Int] m, MonadError String m) => m Int
dequeue'' = do queue <- get
               if null queue
                  then throwError "Queue is empty"
                  else do put $ tail queue
                          return $ head queue

--size :: (Monad m, MonadState [Int] m) => m Int
size = do queue <- get
          return $ length queue

noRepeatedElemAdv proceed arg =
  do queue <- get
     if elem arg queue
        then return ()
        else proceed arg

type M'' = AOT (StateT [Int] (ErrorT String Identity))

runM'' :: M'' a -> Either String a
runM'' c = runIdentity $ runErrorT $ evalStateT (runAOT c) []

-- To use StableNames successfully we need to annotate the type of sqrtM
enqueueTag = $newTag

enqueue''' :: (Monad m, MonadState [Int] m) => Function Int (m ())
enqueue''' = mkFunction (\ n -> do queue <- get; put $ queue ++ [n]) enqueueTag

--program :: M'' Int
program n m =
  do deploy (aspect (pcCall enqueue''') noRepeatedElemAdv)
     enqueue''' # n
     enqueue''' # m
     size