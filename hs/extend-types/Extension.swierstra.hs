{-# LANGUAGE TypeOperators #-}

-- import qualified Base

data Expr f = In (f (Expr f))
data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
    evalAlgebra :: f Value -> Value

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Value
eval expr = foldExpr evalAlgebra expr

data Value = Bottom
           | Constant Int
           | Closure Name (() -> Value)

instance Show Value where
    show Bottom = "<bottom>"
    show (Constant i) = show i
    show (Closure _ _) = "<closure>"

-- Have to modify base

type Name = String

data Bot e = Bot

instance Functor Bot where
    fmap f Bot = Bot

instance Eval Bot where
    evalAlgebra Bot = Bottom

data Con e = Con Int

instance Functor Con where
    fmap f (Con x) = Con x

instance Eval Con where
    evalAlgebra (Con x) = Constant x

data Lam e = Lam Name e

instance Functor Lam where
    fmap f (Lam n b) = Lam n (f b)

instance Eval Lam where
    evalAlgebra (Lam n b) = Closure n (\_ -> eval b)

-- Extension

type Principal = Int

data Facet e = Facet Principal e e
instance Functor Facet where
    fmap f (Facet p t1 t2) = Facet p (f t1) (f t2)

-- FacetV :: Principal -> Value -> Value -> Value

-- eval (Facet p t1 t2) = (FacetV p (feval t1) (feval t2))

-- Tests

term00 :: Expr Bot
term00 = In Bot

term0 :: Expr (Bot :+: Lam)
term0 = In (Inr (Lam "x" (In (Inl Bot))))

-- term1 :: Expr (Facet :+: (Bot :+: Lam))
-- term1 = (Facet 0 (Lam "x" (In (Inr (In (Inl Bot))))) (In (Inr (In (Inl Bot)))))

-- term2 :: Expr ( :+: Facet)
-- term2 = In (Inl (Lam "y" (In (Inr (Facet 0 (In (Inl (Lam "x" (In (Inl Bot))))) (In (Inl Bot)))))))
