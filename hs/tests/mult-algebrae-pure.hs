{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverlappingInstances #-}

data Expr f = In (f (Expr f))

-- instance (Functor f, Show a, Show (f String)) => Show (Term f a) where
--   show t = foldTerm showPure showImpure t
--    where showPure a    = "(Pure " ++ show a ++ ")"
--          showImpure fb = "(Impure " ++ show fb ++ ")"

-- instance (Show (f String), Show (g String)) =>  Show ((f :+: g) String) where
--   show (Inl r) = show r
--   show (Inr r) = show r

-- instance Functor g => Functor (Term g) where
--   fmap f (Pure x)    = Pure (f x)
--   fmap f (Impure t)  = Impure (fmap (fmap f) t)

-- instance Functor g => Monad (Term g) where
--   return x          = Pure x
--   (Pure x)   >>= f  = f x
--   (Impure t) >>= f  = Impure (fmap (>>= f) t)

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 6 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance (Functor f) => f :<: f where
 inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
 inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
 inj = Inr . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

------------------------------------------------------------------------------------
-- "Middleware"

-- exec :: Exec f => Term f a -> b
-- exec = foldTerm return execAlgebra

-- exec :: Exec f => Term f Value -> M Value
-- exec = foldTerm return execAlgebra

-- run :: Exec f => Term f Value -> (Value, Store)
-- run t = runIdentity $ flip runStateT [] $ flip evalStateT [] $ runAOT $
--         do exec t

-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Boolean language

runBool :: EvalBool f => Expr f -> Bool
runBool = foldExpr evalBool

class Functor f => EvalBool f where
  evalBool :: f Bool -> Bool

instance (EvalBool f, EvalBool g) => EvalBool (f :+: g) where
  evalBool (Inl r) = evalBool r
  evalBool (Inr r) = evalBool r

-- True/False

data Tru e = Tru deriving Functor
data Fals e = Fals deriving Functor

instance EvalBool Tru where
    evalBool Tru = True

instance EvalBool Fals where
    evalBool Fals = False

true :: (Tru :<: f) => Expr f
true = inject Tru

false :: (Fals :<: f) => Expr f
false = inject Fals

-- If

data If e = If e e e deriving Functor

instance EvalBool If where
    evalBool (If True a b) = a
    evalBool (If False a b) = b

if' :: (If :<: f) => Expr f -> Expr f -> Expr f -> Expr f
if' c a b = inject $ If c a b

p2 :: Expr (If :+: Tru :+: Fals)
p2 = if' true true false

-----------------------------------------------------------------------------------
-- Arithmetic language

runInt :: EvalInt f => Expr f -> Int
runInt = foldExpr evalInt

class Functor f => EvalInt f where
  evalInt :: f Int -> Int

instance (EvalInt f, EvalInt g) => EvalInt (f :+: g) where
  evalInt (Inl r) = evalInt r
  evalInt (Inr r) = evalInt r

-- Constant term

data Con e = Con Int deriving Functor

con :: (Con :<: f) => Int -> Expr f
con i = inject (Con i)

instance EvalInt Con where
  evalInt (Con x) = x

p1 :: Expr Con
p1 = con 10

-- Plus term

data Plus e = Plus e e deriving Functor

plus :: (Plus :<: f) => Expr f -> Expr f -> Expr f
plus x y = inject (Plus x y)

instance EvalInt Plus where
  evalInt (Plus x y) = x + y

p6 :: Expr (Plus :+: Con)
p6 = plus (con 10) (con 20)

-----------------------------------------------------------------------------------
-- Combined language: CL

data Value = Bool Bool
           | Int Int
           deriving Show

class Functor f => EvalCL f where
  evalCL :: f Value -> Value

runCL :: EvalCL f => Expr f -> Value
runCL = foldExpr evalCL

instance (EvalCL f, EvalCL g) => EvalCL (f :+: g) where
  evalCL (Inl r) = evalCL r
  evalCL (Inr r) = evalCL r

instance EvalCL Con where
    evalCL (Con x) = Int x

instance EvalCL Tru where
    evalCL Tru = Bool True

instance EvalCL If where
    evalCL (If (Bool True) a b) = a
    evalCL (If (Bool False) a b) = b


p7 :: Expr (If :+: Tru :+: Con)
p7 = if' true (con 1) (con 3)
