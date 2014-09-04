{-# LANGUAGE MultiParamTypeClasses #-}

--import qualified Base

-- Need to rework the base

type Name = String

data Term e = Bot
            | Con Int
            | Lam Name e
   deriving Show

data Value e = Bottom
             | Constant Int
             | Closure Name e
   deriving Show

-- instance Eval Term Value where
--     eval (In Bot) = (Rn Bottom)

eval :: Term e -> Value e
eval Bot           = Bottom
eval (Con i)       = (Constant i)
eval (Lam x v)     = (Closure x v)

-- Extension

type Principal = Int

data FTerm e = Facet Principal e e
   deriving Show

data FValue e = FacetV Principal e e

data Expr e = In (e (Expr e))
data Result e = Rn (e (Result e))

-- type BothTerm e = Either (FTerm e) (Term e)
-- type Result e = Either (FValue e) (Value e)


-- Can't find a way to type this thing
-- feval :: (Expr e) -> (Result e)
-- feval (In (Facet p t1 t2)) = Rn (FacetV p (feval t1) (feval t2))
-- feval (In t) = eval t

-- class Eval e f where
--     eval :: Expr e -> Result f

-- Tests

-- All typecheck
term00 = Bot
term0 = (Lam "x" Bot)
term1 = (Facet 0 (Lam "x" Bot) Bot)
term2 = (Lam "y" (Facet 0 (Lam "x" Bot) Bot))
