{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.State
import "effective-aspects" AOP.Default

import Debug.Trace

data Term f a =
    Pure a
  | Impure (f (Term f a))   

instance (Functor f, Show a, Show (f String)) => Show (Term f a) where
  show t = foldTerm showPure showImpure t
   where showPure a    = "(Pure " ++ show a ++ ")"
         showImpure fb = "(Impure " ++ show fb ++ ")"

instance (Show (f String), Show (g String)) =>  Show ((f :+: g) String) where
  show (Inl r) = show r
  show (Inr r) = show r         

instance Functor g => Functor (Term g) where
  fmap f (Pure x)    = Pure (f x)
  fmap f (Impure t)  = Impure (fmap (fmap f) t)

instance Functor g => Monad (Term g) where
  return x        = Pure x
  (Pure x)   >>= f  = f x
  (Impure t) >>= f  = Impure (fmap (>>= f) t)

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

inject :: (g :<: f) => g (Term f a) -> Term f a
inject = Impure . inj

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure _ (Pure x)  = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

------------------------------------------------------------------------------------
-- "Middleware"

-- exec :: Exec f => Term f a -> Identity a
-- exec = foldTerm return execAlgebra

-- associated types?
data Value = 
    IntV Int
  | BoolV Bool
  | ClosureV Name (M Value) Environment
 deriving Typeable

instance Show Value where
  show (IntV i)  = "(IntV " ++ show i ++ ")"
  show (BoolV b) = "(BoolV " ++ show b ++ ")"
  show (ClosureV name _ env) = "(ClosureV (" ++ name ++ ") Env: " ++ show env ++ ")"

type M = AOT (StateT Environment Identity)

exec :: Exec f => Term f Value -> M Value
exec = foldTerm return execAlgebra

type Environment = [(Name, Value)]

class Functor f => Exec f where
  execAlgebra :: f (M Value) -> M Value

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra (Inl r) = execAlgebra r
  execAlgebra (Inr r) = execAlgebra r

appDTag = 10

run :: Exec f => Term f Value -> Value
run t = runIdentity $ flip evalStateT [] $ runAOT $
        do deploy (aspect pcTrue logAdv)
           exec t

pcTrue :: Monad m => PC m a (m b)
pcTrue = PC $ return $ \jp -> trace "Pointcut return True" $ return True

pcTag' tag typ = pcType typ `pcAnd` pcTag tag            

logAdv :: Monad m => Advice m a b
logAdv proceed arg = do result <- proceed arg
                        trace "Applying fun" $ return ()
                        return result -- return (IntV 100) works

------------------------------------------------------------------------------------

data IntD a = Int_ a  deriving Functor

instance Show (IntD String) where
  show (Int_ a) = "(Int_ " ++ a ++ ")"

instance Show a => Show (IntD a) where
  show (Int_ a) = "(Int_ " ++ show a ++ ")"

int :: (IntD :<: f) => Int -> Term f Value
int i = inject (Int_ (Pure (IntV i)))

instance Exec IntD where
  execAlgebra (Int_ a) = a

p1 :: Term IntD Value
p1 = int 10
data BoolD a = Bool_ a deriving Functor

instance Show (BoolD String) where
    show (Bool_ a) = "(Bool_ " ++ a ++ ")"

bool :: (BoolD :<: f) => Bool -> Term f Value
bool b = inject (Bool_ (Pure (BoolV b)))

instance Exec BoolD where
  execAlgebra (Bool_ a) = a

p2 :: Term BoolD Value
p2 = bool True

data AddIntD e = Add e e deriving Functor

add x y = inject (Add x y)

instance Exec AddIntD where
  execAlgebra (Add x y) = do { IntV x' <- x; IntV y' <- y; return (IntV (x'+y')) }

p6 :: Term (IntD :+: AddIntD) Value
p6 = add (int 10) (int 20)

type Name = String

data VarD x = Var Name deriving Functor

var x = inject (Var x)

instance Exec VarD where
  execAlgebra (Var x) = do env <- get
                           case lookup x env of
                             Just v -> return v
                             Nothing -> error $ "Unknown variable" ++ x

data LamD e = Lam Name e deriving Functor

lam x t = inject (Lam x t)

instance Exec LamD where
  execAlgebra (Lam x t) = do env <- get
                             return (ClosureV x t env)

p8 :: Term (LamD :+: VarD :+: IntD) Value
p8 = lam "x" (int 1)

p7 :: Term (LamD :+: IntD :+: AddIntD :+: VarD) Value
p7 = lam "x" (add (var "x") (var "x"))

p9 :: Term (AppD :+: LamD :+: VarD :+: IntD :+: AddIntD) Value
p9 = app (lam "x" (add (var "x") (int 2))) (int 2)

data AppD e = App e e deriving (Functor, Typeable)

app fun arg = inject (App fun arg)

openAppD :: AppD (M Value) -> M Value
openAppD (App fun arg) =
            do (ClosureV name body env) <- fun
               a <- arg
               oldEnv <- get       
               put (("x", a):env)
               result <- body
               put oldEnv
               return result

instance Exec AppD where
  execAlgebra arg = openAppD # arg

let' name value body = app (lam name body) value

p10 :: Term (AppD :+: LamD :+: VarD :+: IntD :+: AddIntD) Value
p10 = let' "x" (int 10) (add (var "x") (var "x"))
