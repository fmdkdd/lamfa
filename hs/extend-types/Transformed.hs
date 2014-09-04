module Main (main) where
 
type Name = String
 
data Term = Bot
          | Con Int
          | Lam Name Term
          | Facet Principal Term Term
          deriving Show
 
data Value = Bottom
           | Constant Int
           | Closure Name Term
           | FacetV Principal Value Value
           deriving Show
 
eval :: Term -> Value
eval (Bot) = Bottom
eval (Con i) = (Constant i)
eval (Lam x v) = (Closure x v)
eval (Facet p t1 t2) = FacetV p (eval t1) (eval t2)
term0 = (Lam "x" Bot)
