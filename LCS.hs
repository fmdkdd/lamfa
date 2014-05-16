--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LCS where

import "mzv" Control.Monad.State
import "mzv" Control.Monad.Identity
import "mzv" Control.Monad.Mask
import "mzv" Control.Monad.Views
--import Data.Typeable

--import Debug.Trace

type Name      = String
type Address   = Int

data Term = Bot
          | Con Int
          | Bol Bool
          | Var Name
          | Lam Name Term
          | App Term Term
          | Ref Term
          | Deref Term
          | Assign Term Term
          | If Term Term Term
          | Let Name Term Term
          | Seq Term Term
   deriving Show

data Value = Error String
           | Bottom
           | Constant Int
           | Boolean Bool
           | Address Int
           | Closure Name Term Environment
-- deriving Typeable

instance Eq Value where
  Error s1 == Error s2               = s1 == s2
  Bottom == Bottom                   = True
  Constant i1 == Constant i2         = i1 == i2
  Boolean b1 == Boolean b2           = b1 == b2
  Address i1 == Address i2           = i1 == i2
  Closure _ _ _ == Closure _ _ _     = False

instance Show Value where
  show (Error s)            = "<Error> " ++ show s
  show Bottom               = "<bottom>"
  show (Constant i)         = show i
  show (Boolean b)          = show b
  show (Address i)          = "<address> " ++ show i
  show (Closure "x" (Lam "y" (Var "x")) _) = show True
  show (Closure "x" (Lam "y" (Var "y")) _) = show False
  show (Closure _ _ _)      = "<closure>"

type Environment = [(Name, Value)]
type Store = [(Address, Value)]

-- newtype EnvironmentT m a = EnvironmentT { aEnv :: (StateT Environment m) a }
--     deriving (Monad, MonadTrans)

-- runEnvironmentT :: EnvironmentT m a -> Environment -> m (a, Environment)
-- runEnvironmentT (EnvironmentT env) = runStateT env

-- newtype StoreT m a = StoreT { aStore :: (StateT Store m) a }
--     deriving (Monad, MonadTrans)

-- runStoreT :: StoreT m a -> Store -> m (a, Store)
-- runStoreT (StoreT env) = runStateT env

-- instance MonadState Environment (StateT Store m) where
--     get = lift get
--     put = lift . put

-- type M = StateT Environment (StateT Store Identity)
--type M = StateT InterpState Identity

-- data InterpState = InterpState { env :: Environment, store :: Store }
--   deriving Show

-- class MState m where
--   getEnv :: m Environment

-- instance Monad m => MonadProgCounter (ProgCounterT m) where
--   getProgCounter = ProgCounterT $ StateT $ \pc -> return (pc, pc)

-- class MonadState Environment where
--     get

--instance MonadState InterpState

--instance

data EnvTag = EnvTag
data StoreTag = StoreTag

type M = TStateT EnvTag Environment (TStateT StoreTag Store Identity)

-- runM :: M Value -> Environment -> Store -> ((Value, Environment), Store)
--runM :: M Value -> InterpState -> (Value, InterpState)
runM :: M Value -> Environment -> Store -> ((Value, Environment), Store)
runM m env store = runIdentity $ runTStateT store (runTStateT env m)

ret v = return (v, return (), return ())

interp :: forall m e s.
          (Monad m,
           TWith EnvTag e m, MonadState Environment e,
           TWith StoreTag s m, MonadState Store s)
          => Term -> m (Value, e (), s ())
interp Bot         = ret $ Bottom
interp (Con i)     = ret $ Constant i

interp (Var x)     = do env <- getv viewEnv
                        ret $ envLookup x env
                     where viewEnv = structure EnvTag :: e :><: m

interp (Lam x v)   = do env <- getv viewEnv
                        ret $ Closure x v env
                     where viewEnv = structure EnvTag :: e :><: m

interp (App t u) = do (f, _, _) <- interp t :: m (Value, e (), s ())
                      (a, _, _) <- interp u :: m (Value, e (), s ())
                      apply f a

interp (Ref t) =
    do (v, _, _) <- interp t :: m (Value, e (), s())
       store <- getv viewStore
       let addr = length store
       putv viewStore $ (addr,v):store
       ret $ Address addr
    where viewStore = structure StoreTag :: s :><: m

interp (Deref t) =
    do (v, _, _) <- interp t :: m (Value, e (), s())
       deref v

interp (Assign l r) =
  do (lv, _, _) <- interp l :: m (Value, e (), s())
     (rv, _, _) <- interp r :: m (Value, e (), s())
     assign lv rv

-- desugaring
interp (Let id namedExpr body) = interp $ App (Lam id body) namedExpr
interp (Seq left right)        = interp $ Let "freevar" left right
interp (If cond thn els)       = interp $ App
                                           (App
                                            (App cond (Lam "d" thn))
                                            (Lam "d" els))
                                           (Lam "x" (Var "x"))
interp (Bol True)              = interp $ Lam "x" (Lam "y" (Var "x"))
interp (Bol False)             = interp $ Lam "x" (Lam "y" (Var "y"))


-- helpers

deref :: forall m e s.
         (Monad m,
          TWith EnvTag e m, MonadState Environment e,
          TWith StoreTag s m, MonadState Store s)
         => Value -> m (Value, e (), s ())
deref Bottom      = ret Bottom
deref (Address a) = do store <- getv viewStore
                       ret $ storeLookup a store
                    where viewStore = structure StoreTag :: s :><: m

assign :: forall m e s.
         (Monad m,
          TWith EnvTag e m, MonadState Environment e,
          TWith StoreTag s m, MonadState Store s)
         => Value -> Value -> m (Value, e (), s ())
assign Bottom _          = ret Bottom
assign (Address a) right = do store <- getv viewStore
                              putv viewStore $ storeReplace a right store
                              ret right
                           where viewStore = structure StoreTag :: s :><: m


apply :: forall m e s.
         (Monad m,
          TWith EnvTag e m, MonadState Environment e,
          TWith StoreTag s m, MonadState Store s)
         => Value -> Value -> m (Value, e (), s ())
apply Bottom _               = ret Bottom
apply (Closure x body env) v = do putv viewEnv ((x,v):env)
                                  interp body
                                where viewEnv = structure EnvTag :: e :><: m

-- other helpers
envLookup :: Name -> Environment -> Value
envLookup x env = case lookup x env of
                    Just v -> v
                    Nothing -> Error $ "unbound " ++ show x

storeLookup :: Address -> Store -> Value
storeLookup a store = case lookup a store of
                        Just v -> v
                        Nothing -> Error $ "not in store " ++ show a

storeReplace :: Address -> Value -> Store -> Store
storeReplace a v [] = []
storeReplace a v ((b,w):s) = if a == b then ((a,v):s)
                             else (b,w):(storeReplace a v s)

-- testing

-- use implicit parameters??

interpWrap :: forall m e s.
              (Monad m,
               TWith EnvTag e m, MonadState Environment e,
               TWith StoreTag s m, MonadState Store s)
              => Term -> m Value
interpWrap t = do (v, _, _) <- interp t :: m (Value, e (), s ())
                  return v

--test :: Term -> Environment -> Store -> IO ()
test t env store = print $ runM (interpWrap t) env store

testDefault t = test t [] []

-- Tests, we skip them for now!

term0 = (App (Lam "x" (Var "x")) (Con 10))
termBot = (App Bot (Con 1))
termStore = (Deref (App (Lam "x" (App (Lam "y" (Var "x"))
                                      (Assign (Var "x") (Con 2))))
                        (Ref (Con 1))))

termLet = (Let "x" (Con 1) (Var "x"))
termSeq = (Let "x" (Ref (Con 1))
           (Seq
            (Assign (Var "x") (Con 2))
            ((Deref (Var "x")))))




-- assert facetTest1 == (Facet (1,True) (Con 42) (Con 24))
--facetTest1 = (If (Facet 1 (Bol True) (Bol False)) (Con 42) (Con 24))

-- assert facetTest1 ==
facetTest2 = (Ref (Con 843))

facetTest3 = (Let "x" (Ref (Bol True))
              (Assign (Var "x") (Bol False)))

fentonTestRaw = (Let "x" (Ref (Bol True))
                 (Let "y" (Ref (Bol True))
                  (Let "z" (Ref (Bol True))
                   (Seq
                    (Seq
                     (If (Deref (Var "x"))
                      (Assign (Var "y") (Bol False))
                      Bot)
                     (If (Deref (Var "y"))
                      (Assign (Var "z") (Bol False))
                      Bot))
                    (Deref (Var "z"))))))

-- fentonTest = (Let "x" (Ref (Facet 1 (Bol True) Bot))
--               (Let "y" (Ref (Bol True))
--                (Let "z" (Ref (Bol True))
--                 (Seq
--                  (Seq
--                   (If (Deref (Var "x"))
--                    (Assign (Var "y") (Bol False))
--                    Bot)
--                   (If (Deref (Var "y"))
--                    (Assign (Var "z") (Bol False))
--                    Bot))
--                  (Deref (Var "z"))))))

-- let x = ref (<1 ? true : ⟂>) in (
--   let y = ref true in (
--     let z = ref true in (
--       if !x then y := false;  # y = <1 ? false : true>
--       if !y then z := false;  # z = <1 ? true : false>
--         !z)))
