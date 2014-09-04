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
    BotV
  | TrueV
  | FalseV
  | IntV Int
  | BoolV Bool
  | AddrV Int
  | ClosureV Name (M Value) Environment
 deriving Typeable

instance Show Value where
  show BotV = "BotV"
  show TrueV = "TrueV"
  show FalseV = "FalseV"
  show (IntV i)  = "(IntV " ++ show i ++ ")"
  show (BoolV b) = "(BoolV " ++ show b ++ ")"
  show (AddrV a) = "(AddrV " ++ show a ++ ")"
  show (ClosureV name _ env) = "(ClosureV " ++ name ++ ")" -- Env: " ++ show env ++ ")"

type Name = String
type Address   = Int

type Environment = [(Name, Value)]
type Store = [(Address, Value)]

type M = AOT (StateT Environment (StateT Store Identity))

exec :: Exec f => Term f Value -> M Value
exec = foldTerm return execAlgebra

class Functor f => Exec f where
  execAlgebra :: f (M Value) -> M Value

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra (Inl r) = execAlgebra r
  execAlgebra (Inr r) = execAlgebra r

-- appDTag = 10

run :: Exec f => Term f Value -> (Value, Store)
run t = runIdentity $ flip runStateT [] $ flip evalStateT [] $ runAOT $
        do deploy (aspect (pcCall openAppD) logAdv)
           exec t

-- pcTrue :: Monad m => PC m a (m b)
-- pcTrue = PC $ return $ \jp -> trace "Pointcut return True" $ return True

-- pcTag' tag typ = pcType typ `pcAnd` pcTag tag

--logAdv :: Monad m => Advice m a b
logAdv proceed arg = do result <- proceed arg
                        trace "Applying fun" $ return (IntV 100)
                        --return result

------------------------------------------------------------------------------------

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Bot term

data BotD a = Bot_ deriving Functor

instance Show a => Show (BotD a) where
  show Bot_ = "(Bot_)"

bot :: (BotD :<: f) => Term f Value
bot = inject Bot_

instance Exec BotD where
  execAlgebra Bot_ = return BotV

p0 :: Term BotD Value
p0 = bot

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Constant term

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

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Bool term

data BoolD a = Bool_ a deriving Functor

instance Show (BoolD String) where
    show (Bool_ a) = "(Bool_ " ++ a ++ ")"

bool :: (BoolD :<: f) => Bool -> Term f Value
bool b = inject (Bool_ (Pure (BoolV b)))

instance Exec BoolD where
  execAlgebra (Bool_ a) = a

p2 :: Term BoolD Value
p2 = bool True

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Add int term

-- data AddIntD e = Add e e deriving Functor

-- add x y = inject (Add x y)

-- instance Exec AddIntD where
--   execAlgebra (Add x y) = do { IntV x' <- x; IntV y' <- y; return (IntV (x'+y')) }

-- p6 :: Term (IntD :+: AddIntD) Value
-- p6 = add (int 10) (int 20)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Variable term

data VarD x = Var Name deriving Functor

instance Show (VarD String) where
  show (Var a) = "(Var " ++ a ++ ")"

instance Show a => Show (VarD a) where
  show (Var a) = "(Var " ++ show a ++ ")"

var x = inject (Var x)

instance Exec VarD where
  execAlgebra (Var x) = do env <- get
                           case lookup x env of
                             Just v -> return v
                             Nothing -> error $ "Unknown variable " ++ x

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Lambda term

data LamD e = Lam Name e deriving Functor

instance Show (LamD String) where
  show (Lam n a) = "(Lam " ++ n ++ ". " ++ a ++ ")"

instance Show a => Show (LamD a) where
  show (Lam n a) = "(Lam " ++ n ++ ". " ++ show a ++ ")"

lam x t = inject (Lam x t)

instance Exec LamD where
  execAlgebra (Lam x t) = do env <- get
                             return (ClosureV x t env)

p8 :: Term (LamD :+: VarD :+: IntD) Value
p8 = lam "x" (int 1)

-- p7 :: Term (LamD :+: IntD :+: AddIntD :+: VarD) Value
-- p7 = lam "x" (add (var "x") (var "x"))

-- p9 :: Term (AppD :+: LamD :+: VarD :+: IntD :+: AddIntD) Value
-- p9 = app (lam "x" (add (var "x") (int 2))) (int 2)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Application term

data AppD e = App e e deriving (Functor, Typeable)

instance Show (AppD String) where
  show (App a b) = "(App " ++ a ++ " " ++ b ++ ")"

instance Show a => Show (AppD a) where
  show (App a b) = "(App " ++ show a ++ " " ++ show b ++ ")"

app fun arg = inject (App fun arg)

openAppD :: AppD (M Value) -> M Value
openAppD (App fun arg) =
    do (ClosureV name body env) <- fun
       a <- arg
       oldEnv <- get
       put ((name, a):env)
       result <- body
       put oldEnv
       return result

instance Exec AppD where
  execAlgebra arg = openAppD # arg

p10 :: Term (AppD :+: BotD) Value
p10 = app bot bot

p11 :: Term (AppD :+: LamD :+: VarD :+: BotD) Value
p11 = app (lam "x" (var "x")) bot

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Ref term

data RefD e = Ref e deriving Functor

instance Show (RefD String) where
  show (Ref a) = "(Ref " ++ a ++ ")"

instance Show a => Show (RefD a) where
  show (Ref a) = "(Ref " ++ show a ++ ")"

ref t = inject (Ref t)

instance Exec RefD where
  execAlgebra (Ref t) = do store <- lift $ lift $ get
                           v <- t
                           let addr = length store
                           (lift . lift. put) $ (addr,v):store
                           return (AddrV addr)

p12 :: Term (RefD :+: BotD) Value
p12 = ref bot

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Deref term

data DerefD e = Deref e deriving Functor

instance Show (DerefD String) where
  show (Deref a) = "(Deref " ++ a ++ ")"

instance Show a => Show (DerefD a) where
  show (Deref a) = "(Deref " ++ show a ++ ")"

deref t = inject (Deref t)

instance Exec DerefD where
  execAlgebra (Deref t) = do v <- t
                             case v of
                               BotV -> return BotV
                               (AddrV a) -> do store <- lift $ lift $ get
                                               case lookup a store of
                                                 Just v -> return v
                                                 Nothing -> error $ "not in store " ++ show a

p13 :: Term (DerefD :+: RefD :+: BotD) Value
p13 = deref (ref bot)

p14 :: Term (DerefD :+: BotD) Value
p14 = deref bot

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Assign term

data AssignD e = Assign e e deriving Functor

instance Show (AssignD String) where
  show (Assign a b) = "(Assign " ++ a ++ " " ++ b ++ ")"

instance Show a => Show (AssignD a) where
  show (Assign a b) = "(Assign " ++ show a ++ " " ++ show b ++ ")"

assign l r = inject (Assign l r)

instance Exec AssignD where
  execAlgebra (Assign l r) = do lv <- l
                                rv <- r
                                case lv of
                                  BotV -> return BotV
                                  (AddrV a) -> do store <- lift $ lift $ get
                                                  (lift . lift. put) (storeReplace a rv store)
                                                  return rv

storeReplace :: Address -> Value -> Store -> Store
storeReplace a v [] = []
storeReplace a v ((b,w):s) = if a == b then ((a,v):s)
                             else ((b,w):(storeReplace a v s))

p18 :: Term (AssignD :+: RefD :+: IntD) Value
p18 = assign (ref (int 0)) (int 1)

p19 :: Term (AssignD :+: BotD) Value
p19 = assign bot bot

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- True and false (cannot be syntactic sugar without dumb arguments)

data TrueD a = True_ deriving Functor

true :: (TrueD :<: f) => Term f Value
true = inject True_

instance Exec TrueD where
  execAlgebra True_ = return TrueV

data FalseD a = False_ deriving Functor

false :: (FalseD :<: f) => Term f Value
false = inject False_

instance Exec FalseD where
  execAlgebra False_ = return FalseV

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If

data IfD a = If a a a deriving Functor

if' cond then' else' = inject (If cond then' else')

instance Exec IfD where
  execAlgebra (If cond then' else') = do t <- cond
                                         case t of
                                           TrueV -> then'
                                           FalseV -> else'

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Syntactic sugar

-- let
let' name value body = app (lam name body) value

p15 :: Term (AppD :+: LamD :+: VarD :+: IntD) Value
p15 = let' "x" (int 10) (var "x")

-- true, false, if
--true dumb = lam "x" (lam "y" (var "x"))
--false dumb = lam "x" (lam "y" (var "y"))

-- if' cond then' else' = do t <- cond
--                           case t of
--                             TrueV -> then'
--                             FalseV -> else'

p16 :: Term (TrueD :+: IfD :+: IntD) Value
p16 = if' true (int 1) (int 2)

-- seq
seq' t1 t2 = let' "dumb" t1 t2

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Fenton test

fenton :: Term (IfD :+: TrueD :+: FalseD :+: AssignD :+: RefD :+: DerefD :+: AppD :+: LamD :+: VarD :+: BotD) Value
fenton = let' "x" (ref true)
         (let' "y" (ref true)
          (let' "z" (ref true)
           (seq'
            (seq'
             (if' (deref (var "x"))
              (assign (var "y") false)
              bot)
             (if' (deref (var "y"))
              (assign (var "z") false)
              bot))
            (deref (var "z")))))
