{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

type Name = String

data Term e = Bot
            | Con Int
            | Lam Name e
          deriving Show

data Value e v = Bottom
               | Constant Int
               | Closure Name e
          deriving Show

--type Exp = Term Value

--type FExp =

class Eval e v where
    eval :: e -> v

instance Eval (Term e) (Value e v) where
    eval Bot = Bottom
    eval (Con x) = Constant x
    eval (Lam n t) = Closure n t

term0 = Bot
term1 = Lam "x" Bot

type Principal = Int

data FTerm e = Facet Principal (Term e) (Term e)
           deriving Show

data FValue e v = FacetV Principal (Value e v) (Value e v)
            deriving Show

instance Eval (FTerm e) (FValue e v) where
    eval (Facet p t1 t2) = FacetV p (eval t1) (eval t2)
