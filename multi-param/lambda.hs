{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import State

class Eval t v where
    eval :: t -> v

----------
-- λ-calculus

type Name = String

data Term = Var Name
          | Lam Name Term
          | App Term Term
  deriving Show

data Value = Closure Name Term Environment
  deriving Show

type Environment = [(Name, Value)]

type M a = State Environment a

instance Eval Term (M Value) where
    eval (Var x) = get >>= \env ->
                   case lookup x env of
                     Just v -> return v
                     Nothing -> fail $ "Variable " ++ x ++ " not bound"

    eval (Lam x body) = get >>= \env ->
                        return $ Closure x body env

    eval (App f v) = eval f >>= \(Closure x body env) ->
                     eval v >>= \v ->
                     withState ((x,v):env) (eval body)

exp6 :: Term
exp6 = (App (Lam "x" (Var "x")) (Lam "x" (Var "x")))

test6 :: M Value
test6 = eval exp6
