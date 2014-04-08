{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE DeriveDataTypeable #-}

module LCS where

import Control.Monad.State
import Control.Monad.Identity
import Data.Typeable

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

type M = StateT (Environment, Store) Identity

runM :: M Value -> Environment -> Store -> (Value, (Environment, Store))
runM m env store = runIdentity (runStateT m (env, store))

interp :: (Monad m, MonadState (Environment, Store) m) => Term -> m Value
interp Bot         = return Bottom
interp (Con i)     = return $ Constant i
interp (Var x)     = do (env,_) <- get
                        return $ envLookup x env
interp (Lam x v)   = do (env,_) <- get
                        return $ Closure x v env

interp (App t u) =
  do f <- interp t
     a <- interp u
     apply f a

interp (Ref t) =
    do v <- interp t
       (env,store) <- get
       let addr = length store
       put (env,(addr,v):store)
       return $ Address addr

interp (Deref t) =
  do v <- interp t
     deref v

interp (Assign l r) =
  do lv <- interp l
     rv <- interp r
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

deref :: (Monad m, MonadState (Environment, Store) m) => Value -> m Value
deref Bottom      = return Bottom
deref (Address a) = do (_,store) <- get
                       return $ storeLookup a store

assign :: (Monad m, MonadState (Environment, Store) m) => Value -> Value -> m Value
assign Bottom _          = return Bottom
assign (Address a) right = do (env,store) <- get
                              put (env,(storeReplace a right store))
                              return right

apply :: (Monad m, MonadState (Environment, Store) m) => Value -> Value -> m Value
apply Bottom _               = return Bottom
apply (Closure x body env) v = do (_,store) <- get
                                  put ((x,v):env,store)
                                  interp body

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

test :: Term -> Environment -> Store -> IO ()
test t env store = print $ runM (interp t) env store

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
