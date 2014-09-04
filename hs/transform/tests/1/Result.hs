module Main (main) where

type Name = String

data Term = Bot
          | Con Int
          | Lam Name Term
          | P Term Term
          deriving Show

data Value = Bottom
           | Constant Int
           | Closure Name Term
           | Pair Value Value
           deriving Show

eval :: Term -> Value
eval (Bot) = Bottom
eval (Con i) = Constant i
eval (Lam x v) = Closure x v
eval (P t1 t2) = Pair (eval t1) (eval t2)
term0 = (Lam "x" Bot)
term1 = P (Con 1) (Con 2)
