{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             Rank2Types,
             TypeOperators
 #-}

module AOP.Semantics.Membranes.MembraneT (
 Membrane(..),
 MembraneT (..),
 MonadMembrane(..),
 emptyMembraneGraph,
 register,
 unregister,
 runMembraneT,
 CombinationAdvice (..),
 ReplacementAdvice (..),
 AugmentationAdvice (..),
 NarrowingAdvice (..),
 NoAdvice (..),
 AnyAdvice,
 Combination (..),
 Replacement (..),
 Augmentation (..),
 Narrowing (..),
 pcCallGlobal,
 pcTypeGlobal,
 pcCallView
) where

import Debug.Trace
import Data.Maybe

import AOP.Internal.Typeable1Monad
import AOP.Internal.Aspect
import AOP.Internal.AspectDef
import AOP.Internal.AOT
import AOP.Internal.AOTType hiding (run)
import AOP.Internal.Advice
import qualified AOP.Internal.AOTType as AOT (run)
import AOP.Internal.LessGen
import AOP.Semantics.Membranes.MonadMembrane
import AOP.Semantics.Membranes.MembraneGraph

import AOP.Internal.Joinpoint
import AOP.Internal.Pointcut
import Data.Graph.Inductive.Graph
import Control.Monad.Views
import Control.Monad.Zipper
import AOP.Internal.OpenApp
import AOP.Internal.Function
import AOP.Internal.PointcutDef

import Unsafe.Coerce

runMembraneT :: Monad m => MembraneT m a -> (MembraneId, MembraneGraph) -> m a
runMembraneT c g = liftM fst $ runMG c g

belongsToGraph :: MembraneId -> MembraneGraph -> Bool
belongsToGraph mbId g = isJust (fst $ match mbId g)

mbAt :: Monad m => MembraneId -> MembraneT m ()
mbAt mb = mkMembraneT $ \ (currMbId, g) -> return ((), (mb, g))

_currentMembrane :: Monad m => MembraneT m MembraneId
_currentMembrane = mkMembraneT $ \ (mb, g) -> return (mb, (mb, g))

_getGraph :: Monad m => MembraneT m MembraneGraph
_getGraph = mkMembraneT $ \ (mb, g) -> return (g, (mb, g))

_newMembrane :: (Monad m, Monad n) =>  
             (forall t a b. Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b)) ->  
             (forall t a b. Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b)) ->
             t ->
             n :><: AOT (MembraneT m) ->       
             MembraneT m (Membrane m n t)
_newMembrane jpIn jpOut t view = mkMembraneT $ \ (mb, g) ->
                          let newMb = makeMembrane g jpIn jpOut t view
                          in return (newMb, (mb, addMembrane newMb g))

_advise :: (Monad m, Monad n1, Monad n2) => Membrane m n1 t1 -> Membrane m n2 t2 -> (MembraneT m) ()
_advise mb1 mb2 = mkMembraneT $ \ (mb, g) -> return ((), (mb, bindMembranes ((mbId mb1), (mbId mb2)) g))

_advises :: Monad m => MembraneId -> MembraneId -> (MembraneT m) Bool
_advises mb1 mb2 = mkMembraneT $ \ (mb, g) ->
   if (belongsToGraph mb1 g && belongsToGraph mb2 g)
      then return (mb1 `elem` pre g mb2, (mb, g))
      else return (False, (mb, g))

_evalIn :: Monad m => MembraneId -> MembraneT m a -> MembraneT m a
_evalIn mb c = do {currentMbId <- _currentMembrane; mbAt mb; result <- c; mbAt currentMbId; return result}

_lambda_evalIn :: Monad m => (a -> MembraneT m b) -> MembraneId -> a -> MembraneT m b
_lambda_evalIn f mb arg = _evalIn mb (f arg)

unregister :: (Typeable1Monad m, LessGen (a -> b) (c -> (AOT (MembraneT m)) d)) =>
               Aspect (AOT (MembraneT m)) a b c d -> MembraneId ->
               AspectEnv (AOT (MembraneT m)) -> (AOT (MembraneT m)) (AspectEnv (AOT (MembraneT m)))
unregister asp@(Aspect (pc :: PC (AOT (MembraneT m)) a b) adv hnd) mb aenv =
    return (deleteAsp (EAspect asp) aenv)


data CombinationAdvice  = CombinationAdvice
data ReplacementAdvice  = ReplacementAdvice
data AugmentationAdvice = AugmentationAdvice
data NarrowingAdvice    = NarrowingAdvice
type AnyAdvice          = CombinationAdvice
data NoAdvice           = NoAdvice

-- why data and not newtype?
data CanBeRegistered  m a b c = CanBeRegistered (Advice m a b)
data Combination  m a b c  = Combination  (Advice m a b)
data Replacement  m a b c  = Replacement  (Replace m a b)
data Augmentation m a b c  = Augmentation (Augment a b c m)
data Narrowing    m a b c  = Narrowing    (Narrow a b c m)

class Register t constr where
      register :: (Typeable1Monad m, Typeable1Monad n,
                    LessGen (a -> (AOT (MembraneT m)) b) (c -> (AOT (MembraneT m) d))) =>
                    PC n a (n b) ->
                    constr n a b w ->
                    Membrane m n t -> AOT (MembraneT m) ()

instance Register NoAdvice CanBeRegistered

instance Register CombinationAdvice Combination where
  register pc (Combination comAdv) mb@(Membrane _ _ _ _ view)  = _register (aspect (liftViewPC view pc) (liftViewAdv view comAdv)) mb

instance Register ReplacementAdvice Replacement where
  register  pc (Replacement repAdv) mb@(Membrane _ _ _ _ view) = _register (aspect (liftViewPC view pc) (liftViewAdv view (replace repAdv))) mb

instance Register AugmentationAdvice Augmentation where
  register pc (Augmentation augAdv) mb@(Membrane _ _ _ _ view) = _register (aspect (liftViewPC view pc) (liftViewAdv view (augment augAdv))) mb

instance Register NarrowingAdvice Narrowing where
  register pc (Narrowing narAdv) mb@(Membrane _ _ _ _ view) = _register (aspect (liftViewPC view pc) (liftViewAdv view (narrow narAdv))) mb

_register :: (Typeable1Monad m, Typeable1Monad n, 
             LessGen (a -> AOT (MembraneT m) b) (c -> AOT (MembraneT m) d)) =>
             Aspect (AOT (MembraneT m)) a (AOT (MembraneT m) b) c d -> 
             Membrane m n t -> AOT (MembraneT m) ()
_register (asp@(Aspect pc adv hnd) :: Aspect (AOT (MembraneT m)) a (AOT (MembraneT m) b) c d)
          (mbReg@(Membrane id jpin jpout t view :: Membrane m n t)) =
    let 
        pcMb mbRegId = (PC $ return $ \ jp -> do
           mbAppId <- currentMembrane
           mbApply <- advises mbRegId mbAppId
           -- TO DO: prove type safety of using unsafeCoerce inside getJpOut (?).
           outputFilter <- getJpOut
           case outputFilter jp of
              Nothing  -> return False
              Just jp' -> if (not mbApply)
                            then return False 
                            else let inputJp = jpin jp'
                                 in case inputJp of
                                    Nothing   -> return False
                                    Just jp'' -> (evalIn mbReg (runPC pc jp''))) :: PC (AOT (MembraneT m)) a ((AOT (MembraneT m)) b)

        advMb mbRegId proceed arg = do {mbAppId <- currentMembrane; evalIn mbReg (adv (lambda_evalIn proceed mbAppId) arg)}
    in mkAOT $ \ aenv -> do
       let newAspect = (Aspect (pcMb id) (advMb id) hnd)
       return ((), EAspect newAspect : aenv)

liftViewAdv :: (Typeable1Monad n , Typeable1Monad m) => n :><: AOT (MembraneT m) -> Advice n a b -> Advice (AOT (MembraneT m)) a b
liftViewAdv view adv proceed arg = bifrom view $ adv (\ a -> bito view (proceed a)) arg

liftViewPC :: (Typeable1Monad n, Typeable1Monad m) =>
              n :><: AOT (MembraneT m) ->
              PC n a (n b)             ->
              PC (AOT (MembraneT m)) a ((AOT (MembraneT m)) b)
liftViewPC view pc = PC $ return $ \ jp -> bifrom view $ runPC pc (liftViewJp (inverse view) jp)

liftViewJp :: (Typeable1Monad n, Typeable1Monad m) => AOT (MembraneT m) :><: n -> Jp (AOT (MembraneT m)) a b -> Jp n a b
liftViewJp view (Jp f t a) = Jp (\ arg -> bifrom view (f arg)) t a

unliftViewPC :: (Typeable1Monad n, Typeable1Monad m) =>
              n :><: AOT (MembraneT m) ->
              PC (AOT (MembraneT m)) a ((AOT (MembraneT m)) b) ->
              PC n a (n b)  
unliftViewPC view pc = PC $ return $ \ jp -> bito view $ runPC pc (unliftViewJp view jp)

unliftViewJp :: (Typeable1Monad n, Typeable1Monad m) => n :><: AOT (MembraneT m) -> Jp n a b -> Jp (AOT (MembraneT m)) a b
unliftViewJp view (Jp f t a) = Jp (\ arg -> bifrom view (f arg)) t a


class PCCallGlobal f where
  pcCallGlobal :: (Typeable1Monad n, Typeable1Monad m, PolyTypeable (a -> AOT (MembraneT m) b)) =>
                  n :><: AOT (MembraneT m)    ->
                  f a ((AOT (MembraneT m)) b) ->
                  PC n a (n b)

instance PCCallGlobal ((->)) where
  pcCallGlobal view fun = unliftViewPC view (pcCall fun)

instance PCCallGlobal Function where
  pcCallGlobal view fun = unliftViewPC view (pcCall fun)


class PCTypeGlobal f where
  pcTypeGlobal :: (Typeable1Monad n, Typeable1Monad m, PolyTypeable (a -> AOT (MembraneT m) b)) =>
                  n :><: AOT (MembraneT m)    ->
                  f a ((AOT (MembraneT m)) b) ->
                  PC n a (n b)

instance PCTypeGlobal ((->)) where
  pcTypeGlobal view fun = unliftViewPC view (pcType fun)

instance PCTypeGlobal Function where
  pcTypeGlobal view fun = unliftViewPC view (pcType fun)


class PCCallView f where
  pcCallView :: (Typeable1Monad n, Typeable1Monad m, Typeable1Monad l, PolyTypeable (a -> AOT (MembraneT m) b)) =>
                  l :><: AOT (MembraneT m)    ->
                  n :><: AOT (MembraneT m)    ->
                  f a (l b) ->
                  PC n a (n b)

instance PCCallView ((->)) where
  pcCallView viewL viewN fun = unliftViewPC viewN (liftViewPC viewL (pcCall fun))

instance PCCallView Function where
  pcCallView viewL viewN fun = unliftViewPC viewN (liftViewPC viewL (pcCall fun))



_getJpOut :: (Monad m) => MembraneT m (Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b))
_getJpOut = mkMembraneT $ \ (currentMb, g) -> let (maybeCtx, _) = match currentMb g in
                                     case maybeCtx of
                                       Nothing -> error "Membrane with given id not found"
                                       Just (_, _, EMembrane mb, _) -> return (unsafeCoerce (jpOut mb), (currentMb, g))

-- | Gets the output filter of the current membrane
getJpOut :: (Monad m) => (AOT (MembraneT m)) (Jp (AOT (MembraneT m)) a b -> Maybe (Jp (AOT (MembraneT m)) a b))
getJpOut = lift _getJpOut


{- Interaction with the standard monad transformers, and with the membrane transformer -}

-- | Interaction with AOT. AOT becomes a MonadMembrane instance when applied on top of a MonadMembrane monad.
instance (Monad m, Typeable1Monad (AOT m)) => MonadMembrane AOT MembraneT m where
         currentMembrane                = mkAOT $ \ aenv -> do mb <- _currentMembrane
                                                               return (mb, aenv)
         getGraph                       = lift _getGraph
         advise mb1 mb2                 = mkAOT $ \ aenv -> do _advise mb1 mb2; return ((), aenv)
         advises mb1 mb2                = mkAOT $ \ aenv -> do result <- _advises mb1 mb2; return (result, aenv)
         evalIn mb c                    = mkAOT $ \ aenv -> do _evalIn (mbId mb) (AOT.run c aenv)
         lambda_evalIn f mb arg         = mkAOT $ \ aenv -> do _lambda_evalIn (\ a' -> AOT.run (f a') aenv) mb arg
         newMembrane jpin jpout t view  = mkAOT $ \ aenv -> do mb <- _newMembrane jpin jpout t view
                                                               return (mb, aenv)

instance Typeable1Monad m => Typeable1 (MembraneT m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "MembraneT" "MembraneT" "MembraneT") [typeOf1 (undefined :: m ())]

data RPair s a = RPair a s
instance Functor (RPair s) where fmap f (RPair a s) = RPair (f a) s

toRP   (a,s)       = RPair a s
fromRP (RPair a s) = (a,s)

instance MonadTrans MembraneT where
         lift ma = mkMembraneT $ \ g -> do
                           a <- ma
                           return (a, g)
         mt       = MT
         unlift f = mkMembraneT $ \ ctx -> f (\m -> runMG m ctx >>= return . toRP) >>= return . fromRP

instance MonadState s m => MonadState s (MembraneT m) where
         get = lift get
         put = lift . put

instance MonadWriter w m => MonadWriter w (MembraneT m) where
    tell     = lift . tell
    listen m = mkMembraneT $ \ctx -> do
                              ((a, ctx'), w) <- listen (runMG m ctx)
                              return ((a, w), ctx')
    pass m   = mkMembraneT $ \ctx -> pass $ do
                                   ((a, f), ctx') <- runMG m ctx
                                   return ((a, ctx'), f)

instance MonadReader r m => MonadReader r (MembraneT m) where
    ask       = lift ask
    local f m = mkMembraneT $ \s -> local f (runMG m s)
