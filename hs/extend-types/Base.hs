module Base where

type Name = String

data Term = Bot
          | Con Int
          | Lam Name Term
   deriving Show

data Value = Bottom
           | Constant Int
           | Closure Name Term
   deriving Show

eval :: Term -> Value
eval Bot           = Bottom
eval (Con i)       = (Constant i)
eval (Lam x v)     = (Closure x v)

-- Tests

term0 = (Lam "x" Bot)
