{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}

import "mtl" Control.Monad.State
import "mtl" Control.Monad.Identity

data Term f = In (f (Term f))

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

inject :: (g :<: f) => g (Term f) -> Term f
inject = In . inj

foldTerm :: Functor f => (f a -> a) -> Term f -> a
foldTerm f (In t) = f (fmap (foldTerm f) t)

-----------------------------------------------------------------------------------
-- Lambda calculus base

type Name = String
type Environment = [(Name, Value)]

data Value = Closure Name (M Value) Environment
           | IntV Int

instance Show Value where
    show (Closure x _ _) = "<closure " ++ x ++ ">"
    show (IntV i) = show i

type M = StateT Environment Identity

class Functor f => Eval f where
  eval :: f (M Value) -> M Value

instance (Eval f, Eval g) => Eval (f :+: g) where
  eval (Inl r) = eval r
  eval (Inr r) = eval r

exec :: Eval f => Term f -> M Value
exec = foldTerm eval

run :: Eval f => Term f -> (Value, Environment)
run t = runIdentity $ runStateT (exec t) []

----------------------------------------------------------------------------------
-- Variable term

data Var e = Var Name deriving Functor

instance Eval Var where
  eval (Var x) = do env <- get
                    case lookup x env of
                      Just v -> return v
                      Nothing -> fail $ "Unknown variable '" ++ x ++ "'"

var :: (Var :<: f) => Name -> Term f
var x = inject (Var x)

p1 :: (Var :<: f) => Term f
p1 = var "x"

-- Lambda term

data Lambda e = Lambda Name e deriving Functor

instance Eval Lambda where
  eval (Lambda x v) = do env <- get
                         return $ Closure x v env

lam :: (Lambda :<: f) => Name -> Term f -> Term f
lam x t = inject (Lambda x t)

p2 :: (Lambda :<: f, Var :<: f) => Term f
p2 = lam "x" (var "x")

-- Apply term

data Apply e = Apply e e deriving Functor

instance Eval Apply where
    eval (Apply fun arg) = do (Closure name body env) <- fun
                              a <- arg
                              oldEnv <- get
                              put ((name, a):env)
                              result <- body
                              put oldEnv
                              return result

app :: (Apply :<: f) => Term f -> Term f -> Term f
app f v = inject (Apply f v)

--p3 :: (Apply :<: f, Lambda :<: f, Var :<: f) => Term f
p3 :: Term (Apply :+: Lambda :+: Var)
p3 = app p2 p2

----------------------------------------------------------------------------------
-- Extensions

data Constant e = Constant Int deriving Functor

instance Eval Constant where
    eval (Constant x) = return $ IntV x

int :: (Constant :<: f) => Int -> Term f
int x = inject (Constant x)

----------------------------------------------------------------------------------
-- Syntactic sugar

let' :: (Apply :<: f, Lambda :<: f) => Name -> Term f -> Term f -> Term f
let' name value body = app (lam name body) value

p15 :: Term (Apply :+: Lambda :+: Var :+: Constant)
p15 = let' "x" (int 10) (var "x")

seq :: (Apply :<: f, Lambda :<: f) => Term f -> Term f -> Term f
seq t1 t2 = let' "_" t1 t2

----------------------------------------------------------------------------------
-- FlowR

data Privilege = Plus | Minus
type Tag = (Name, Privilege)
type Label = [Tag]
data Labels = Labels Label Label
            | Wildcard

data ValueR = ClosureR Name (MR ValueR) Environment Labels
            | ValueR Value Labels

-- instance Show ValueR where
--     show (ValueR v (Labels r s)) = "ValueR " ++ show v ++ " (" ++ showLabel r ++ "," ++ showLabel s ++ ")"

type MR = StateT Labels M

class Functor f => EvalR f where
  evalR :: f (MR ValueR) -> MR ValueR

instance (EvalR f, EvalR g) => EvalR (f :+: g) where
  evalR (Inl r) = evalR r
  evalR (Inr r) = evalR r

execR :: EvalR f => Term f -> MR ValueR
execR = foldTerm evalR

runR :: EvalR f => Term f -> ValueR
runR t = runIdentity $ evalStateT (evalStateT (execR t) Wildcard) []

------------------------------------------------------------
-- New term

data LambdaR e = LambdaR Name Labels e deriving Functor

instance EvalR LambdaR where
    evalR (LambdaR x l v) = do env <- lift $ get
                               return $ ClosureR x v env l

------------------------------------------------------------
-- Instrumentation

instance EvalR Apply where
    evalR (Apply fun arg) = do (ClosureR x t e funL) <- fun
                               (ValueR v argL) <- arg
                               res <- eval (Apply (Closure x t e) v)
                               return $ ValueR res []
--                               return $ ValueR res []

-- urgh ... cannot call original eval since MR is a larger monad than M
