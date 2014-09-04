module Extended where

type Name = String
type Principal = Int            -- new

data Term = Bot
          | Con Int
          | Lam Name Term
          | Facet Principal Term Term -- new
   deriving Show

data Value = Bottom
           | Constant Int
           | Closure Name Term
           | FacetV Principal Value Value -- new
   deriving Show

eval :: Term -> Value
eval Bot           = Bottom
eval (Con i)       = (Constant i)
eval (Lam x v)     = (Closure x v)
eval (Facet p t1 t2) = (FacetV p (eval t1) (eval t2))

-- Tests

term0 = (Lam "x" Bot)
term1 = (Facet 0 (Lam "x" Bot) Bot)
term2 = (Lam "y" (Facet 0 (Lam "x" Bot) Bot))
