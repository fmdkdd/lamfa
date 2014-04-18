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

import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.State

data Term f a =
    Pure a
  | Impure (f (Term f a))   

instance (Functor f, Show a, Show (f String)) => Show (Term f a) where
  show t = foldTerm showPure showImpure t
   where showPure a    = "(Pure " ++ show a ++ ")"
         showImpure fb = "(Impure " ++ show fb ++ ")"


--  (Show ((:+:) IntD BoolD String))
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
  | ClosureV Name (State Environment Value) Environment
 -- deriving Show

instance Show Value where
  show (IntV i)  = "(IntV " ++ show i ++ ")"
  show (BoolV b) = "(BoolV " ++ show b ++ ")"
  show (ClosureV name _ env) = "(ClosureV (" ++ name ++ ") Env: " ++ show env ++ ")"

exec :: Exec f => Term f Value -> State Environment Value
exec = foldTerm return execAlgebra

type Environment = [(Name, Value)]

class Functor f => Exec f where
  execAlgebra :: f (State Environment Value) -> State Environment Value

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra (Inl r) = execAlgebra r
  execAlgebra (Inr r) = execAlgebra r

run :: Exec f => Term f Value -> Value
run t = evalState (exec t) []

------------------------------------------------------------------------------------

data IntD a = Int_ a  deriving Functor

instance Show (IntD String) where
  show (Int_ a) = "(Int_ " ++ a ++ ")"

instance Show a => Show (IntD a) where
  show (Int_ a) = "(Int_ " ++ show a ++ ")"

int :: (IntD :<: f) => Int -> Term f Value
int i = inject (Int_ (Pure (IntV i)))

-- int :: (IntD :<: f) => Int -> Term f Int
-- int i = (Pure i)

instance Exec IntD where
  execAlgebra (Int_ a) = a

p1 :: Term IntD Value
p1 = int 10

-- now booleans!!

data BoolD a = Bool_ a deriving Functor

instance Show (BoolD String) where
    show (Bool_ a) = "(Bool_ " ++ a ++ ")"

bool :: (BoolD :<: f) => Bool -> Term f Value
bool b = inject (Bool_ (Pure (BoolV b)))

instance Exec BoolD where
  execAlgebra (Bool_ a) = a

p2 :: Term BoolD Value
p2 = bool True

-- recall that (Term f) is a monad
-- p3 :: Term BoolD (Bool, Bool)
-- p3 = do b1 <- bool True
--         b2 <- bool False
--         return (b1, b2)
  
-- p4 :: Term (IntD :+: BoolD) (Value, Value)
-- p4 = do b <- bool True
--         i <- int 10
--         return (i, b)

-- p5 :: Term (IntD :+: BoolD) (Value, Value)
-- p5 = do b <- bool True
--         i <- int 10
--         return (IntV 10, b)

-- adding integers!
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

data AppD e = App e e deriving Functor

app fun arg = inject (App fun arg)

instance Exec AppD where
  execAlgebra (App fun arg) =
    do (ClosureV name body env) <- fun
       a <- arg
       oldEnv <- get       
       put (("x", a):env)
       result <- body
       put oldEnv
       return result

let' name value body = app (lam name body) value

p10 :: Term (AppD :+: LamD :+: VarD :+: IntD :+: AddIntD) Value
p10 = let' "x" (int 10) (add (var "x") (var "x"))
