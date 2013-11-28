{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             GeneralizedNewtypeDeriving
 #-}

module AOP.Semantics.EL.ELT (
 ELT (ELT),
 Level,
 MonadEL(..),
 runELT,
) where

import AOP.Internal.Typeable1Monad
import AOP.Internal.Pointcut
import AOP.Internal.Aspect
import AOP.Internal.AOPMonad
import AOP.Internal.AOT
import AOP.Internal.AOTType hiding (run)
import qualified AOP.Internal.AOTType as AOT (run)
import AOP.Semantics.EL.MonadEL
import AOP.Internal.Function


newtype ELT m a = ELT { unELT :: StateT Level m a }
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)

run = runStateT . unELT 
mkELT = ELT . StateT

runELT :: Monad m => ELT m a -> Level -> m a
runELT c l = liftM fst $ run c l

inc :: Monad m => ELT m ()
inc = mkELT $ \ l -> return ((), l+1)

dec :: Monad m => ELT m ()
dec = mkELT $ \ l -> return ((), max (l - 1) 0)

at :: Monad m => Level -> ELT m ()
at l = mkELT $ \ _ -> return ((), l)

-- | ELT produces level-aware functions.
instance (Monad m) => MonadEL (ELT m) where
  current = mkELT $ \ l -> return (l, l)
  up c   = do {inc; result <- c; dec; return result}
  down c = do {dec; result <- c; inc; return result}
  lambda_at f l arg = do n <- current
                         at l
                         result <- f arg
                         at n
                         return result

-- | Interaction with AOT
instance (MonadEL m, Typeable1Monad (AOT m)) => MonadEL (AOT m) where
         current   = lift current
         up m      = mkAOT $ \ aenv -> up (AOT.run m aenv)
         down m    = mkAOT $ \ aenv -> down (AOT.run m aenv)
         lambda_at f l a = mkAOT $ \ aenv -> lambda_at (\a' -> AOT.run (f a') aenv) l a

-- | Semantics of Execution Levels
instance (Typeable1Monad m) => MonadDeploy AOT (ELT m) where
 undeployInEnv asp@(Aspect (pc :: PC (AOT (ELT m)) a b) adv hnd) aenv =
   return (deleteAsp (EAspect asp) aenv)
                   
 deployInEnv (Aspect (pc :: PC (AOT (ELT m)) a b) adv hnd) aenv =
   let 
       pcEL ldep = (PC $ return $ \jp -> do
                            lapp <- current
                            if ldep == lapp
                               then up $ runPC pc jp
                               else return False) :: PC (AOT (ELT m)) a b
       advEL ldep proceed arg = up $ adv (lambda_at proceed ldep) arg
   in do currentLevel <- current
         return (EAspect (Aspect (pcEL currentLevel) (advEL currentLevel) hnd) : aenv)


{-- | Interaction with Typeable and other monad transformers --}

instance Typeable1Monad m => Typeable1 (ELT m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "ELT" "ELT" "ELT") [typeOf1 (undefined :: m ())]

data RPair s a = RPair a s
instance Functor (RPair s) where
  fmap f (RPair a s) = RPair (f a) s

toRP   (a,s)       = RPair a s
fromRP (RPair a s) = (a,s)

instance MonadTrans ELT where
         lift ma = mkELT $ \level -> do
                   a <- ma
                   return (a, level)
         mt       = MT
         unlift f = mkELT $ \ l ->
                    f (\m -> run m l >>= return . toRP) >>= return . fromRP

instance Monad m => MonadState s (ELT (StateT s m)) where
         get = lift get
         put = lift . put

instance MonadWriter w m => MonadWriter w (ELT m) where
    tell     = lift . tell
    listen m = mkELT $ \ l -> do
               ((a, l'), w) <- listen (run m l)
               return ((a, w), l')
    pass m   = mkELT $ \ l -> pass $ do
               ((a, f), l') <- run m l
               return ((a, l'), f)

instance MonadReader r m => MonadReader r (ELT m) where
    ask       = lift ask
    local f m = mkELT $ \ l -> local f (run m l)