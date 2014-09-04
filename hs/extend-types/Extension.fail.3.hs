{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Base

data BaseTerm = Bot
              | Con Int

type Name = String
data Term e = Lam Name e

data BaseValue = Bottom
               | Constant Int

data Value e = Closure Name e

-- data V = forall e. Result e => V e

class Expr t v where
    eval :: Result v => t -> v

instance Expr BaseTerm v where
    eval Bot = Bottom
    eval (Con x) = (Constant x)

instance (Expr e v, Result e) => Expr (Term e) v where
    eval (Lam n t) = (Closure n t)

class Result v where
    red :: v -> String

-- instance Show V where
--     show = red

-- instance Result V where
--     red v = show v

instance Result BaseValue where
    red Bottom = "<bottom>"
    red (Constant x) = show x

instance Result e => Result (Value e) where
    red (Closure n v) = "<closure>"

-- Extension

-- type Principal = Int
-- data FacetTerm e f = Facet Principal e f
-- data FacetValue e f = FacetV Principal e f

-- instance (Eval e, Eval f) => Eval (FacetTerm e f) where
--     eval (Facet p t1 t2) = FacetV p (eval t1) (eval t2)

term00 = Bot
term0 = Lam "x" Bot
-- term1 = Facet 0 (Lam "x" Bot) Bot
-- term2 = Lam "y" (Facet 0 (Lam "x" Bot) Bot)
