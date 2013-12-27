{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LCS where

import Control.Monad.State
import Control.Monad.Identity
import Data.Typeable

import Debug.Trace



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
 deriving Typeable             

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

type M = (StateT Store Identity)

runM :: M Value -> Store -> (Value, Store)
runM m s = runIdentity (runStateT m s)

interp :: (Monad m, MonadState Store m) => (Term, Environment) -> m Value
interp (Bot, e)         = return Bottom
interp ((Con i), e)     = return (Constant i)
interp ((Var x), e)     = return (envLookup x e)
interp ((Lam x v), e)   = return (Closure x v e)

interp ((App t u), e) =
  do f <- interp (t, e)
     a <- interp (u, e)
     apply f a

interp ((Ref t), e) =
    do v <- interp (t, e)
       store <- get
       let addr = length store
       put ((addr,v):store)
       return (Address addr)

interp ((Deref t), e) =
  do v <- interp (t, e)
     deref v

interp ((Assign l r), e) =
  do lv <- interp (l, e)
     rv <- interp (r, e)
     assign lv rv

-- desugaring
interp ((Let id namedExpr body), e) = interp ((App (Lam id body) namedExpr), e)
interp ((Seq left right), e)        = interp ((Let "freevar" left right), e)
interp ((If cond thn els), e)       = interp ((App
                                           (App
                                            (App cond (Lam "d" thn))
                                            (Lam "d" els))
                                           (Lam "x" (Var "x"))), e)
interp ((Bol True), e)              = interp ((Lam "x" (Lam "y" (Var "x"))), e)
interp ((Bol False), e)             = interp ((Lam "x" (Lam "y" (Var "y"))), e)


-- helpers

deref :: (Monad m, MonadState Store m) => Value -> m Value
deref Bottom      = return Bottom
deref (Address a) = do store <- get
                       return (storeLookup a store)

assign :: (Monad m, MonadState Store m) => Value -> Value -> m Value
assign Bottom _          = return Bottom
assign (Address a) right = do store <- get
                              put (storeReplace a right store)
                              return right

apply :: (Monad m, MonadState Store m) => Value -> Value -> m Value
apply Bottom _               = return Bottom
apply (Closure x body env) v = interp (body, ((x,v):env))

-- other helpers
envLookup :: Name -> Environment -> Value
envLookup x env = case (lookup x env) of
                    Just v -> v
                    Nothing -> (Error ("unbound " ++ show x))

storeLookup :: Address -> Store -> Value
storeLookup a store = case (lookup a store) of
                        Just v -> v
                        Nothing -> (Error ("not in store " ++ show a))

storeReplace :: Address -> Value -> Store -> Store
storeReplace a v [] = []
storeReplace a v ((b,w):s) = if a == b then ((a,v):s)
                             else ((b,w):(storeReplace a v s))

-- testing

-- use implicit parameters??

test :: Term -> Environment -> Store -> String
test t env store = show (runM (interp (t, env)) store)

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
