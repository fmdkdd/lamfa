{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import State

class Eval t v where
    eval :: t -> v

----------
-- λ-calculus with references and bottom

type Name = String
type Address = Int

data Term = Var Name
          | Lam Name Term
          | App Term Term
          | Bot
          | Ref Term
          | Deref Term
          | Assign Term Term
  deriving Show

data Value = Closure Name Term Environment
           | Bottom
           | Address Address
  deriving Show

type Environment = [(Name, Value)]
type Store = [(Address, Value)]

data EvalState = EvalState { env :: Environment,
                             store :: Store }
  deriving Show

type M a = State EvalState a


insert :: (Eq k) => k -> v -> [(k,v)] -> [(k,v)]
insert k v [] = [(k,v)]
insert k v ((k',v'):kvs) = if k == k'
                           then (k,v):kvs
                           else insert k v kvs

modifyStore :: (Store -> Store) -> State EvalState ()
modifyStore f = gets store >>= \stor ->
                puts (\s -> s { store = f stor })


instance Eval Term (M Value) where
    eval (Var x) = gets env >>= \env ->
                   case lookup x env of
                     Just v -> return v
                     Nothing -> fail $ "Variable " ++ x ++ " not bound"

    eval (Lam x t) = gets env >>= \env ->
                     return $ Closure x t env

    eval (App f v) = eval f >>= \(Closure x t env) ->
                     eval v >>= \v ->
                     withEnv ((x,v):env) (eval t)
        where withEnv e f = gets env >>= \oldEnv ->
                            puts (\s -> s {env=e})  >>
                            f >>= \res ->
                            puts (\s -> s {env=oldEnv}) >>
                            return res

    eval Bot = return Bottom

    eval (Ref t) = eval t >>= \v ->
                   gets store >>= \stor ->
                   let a = length stor in
                   modifyStore (insert a v) >>
                   return (Address a)

    eval (Deref t) = eval t >>= \(Address a) ->
                     gets store >>= \stor ->
                     case lookup a stor of
                       Just v -> return v
                       Nothing -> fail $ "Not in store " ++ show a

    eval (Assign t1 t2) = eval t1 >>= \(Address a) ->
                          eval t2 >>= \v ->
                          modifyStore (insert a v) >>
                          return v


runEval :: Term -> (Value, EvalState)
runEval t = runState (eval t) defaultState
    where defaultState = EvalState { env = [], store = [] }


-------------
-- Tests

exp6 :: Term
exp6 = (App (Lam "x" (Var "x")) (Lam "x" (Var "x")))

test6 :: (Value, EvalState)
test6 = runEval exp6

exp7 :: Term
exp7 = (App (Lam "x" (Var "x")) Bot)

test7 :: (Value, EvalState)
test7 = runEval exp7

exp8 :: Term
exp8 = (Assign (Ref Bot) (Lam "x" (Var "x")))

test8 :: (Value, EvalState)
test8 = runEval exp8

exp9 :: Term
exp9 = (Deref (Ref Bot))

test9 :: (Value, EvalState)
test9 = runEval exp9
