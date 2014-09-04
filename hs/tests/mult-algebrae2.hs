{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}

import "mtl" Control.Monad.State
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Writer

import Data.List

data Term f = In (f (Term f))

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance (Functor f) => f :<: f where
 inj = id

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 6 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

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

data Value = forall f. Eval f => Closure Name (Term f) Environment
           | IntV Int
           | Bot

instance Show Value where
    show (Closure x _ _) = "<closure " ++ x ++ ">"
    show (IntV i) = show i

type M = WriterT TraceR (StateT Labels (State Environment))
--type M = State Environment

class Functor f => Eval f where
  eval :: Eval g => f (Term g) -> M Value

instance (Eval f, Eval g) => Eval (f :+: g) where
  eval (Inl f) = eval f
  eval (Inr g) = eval g

exec :: Eval f => Term f -> M Value
exec (In t) = eval t

-- exec :: Eval f => Term f -> M Value
-- exec = foldTerm eval

-- run :: Eval f => Term f -> (Value, Environment)
-- run t = runIdentity $ runStateT (exec t) []

-- Run, please
-- runp :: Term (Constant :+: Apply :+: Lambda :+: Var) -> (Value, Environment)
-- runp = run

----------------------------------------------------------------------------------
-- Variable term

data Var e = Var Name deriving Functor

instance Eval Var where
  eval (Var x) = get >>= \env  ->
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
  eval (Lambda x t) = get >>= \env ->
                      return (Closure x t env)

lam :: (Lambda :<: f) => Name -> Term f -> Term f
lam x t = inject (Lambda x t)

p2 :: (Lambda :<: f, Var :<: f) => Term f
p2 = lam "x" p1

-- Apply term

data Apply e = Apply e e deriving Functor

instance Eval Apply where
    eval (Apply fun arg) = exec fun >>= \(Closure x t env) ->
                           exec arg >>= \a ->
                           withEnv ((x,a):env) (exec t)
        where withEnv e f = get >>= \oldEnv ->
                            put e >>
                            f >>= \res ->
                            put oldEnv >>
                            return res

app :: (Apply :<: f) => Term f -> Term f -> Term f
app f v = inject (Apply f v)

p3 :: (Apply :<: f, Lambda :<: f, Var :<: f) => Term f
p3 = app p2 p2

----------------------------------------------------------------------------------
-- Extensions

data Constant e = Constant Int deriving Functor

instance Eval Constant where
    eval (Constant x) = return $ IntV x

int :: (Constant :<: f) => Int -> Term f
int x = inject (Constant x)


data Bottom e = Bottom deriving Functor

instance Eval Bottom where
    eval Bottom = return Bot

bot :: (Bottom :<: f) => Term f
bot = inject Bottom

----------------------------------------------------------------------------------
-- Syntactic sugar

let' :: (Apply :<: f, Lambda :<: f) => Name -> Term f -> Term f -> Term f
let' name value body = app (lam name body) value

p15 :: (Apply :<: f, Lambda :<: f, Var :<: f, Constant :<: f) => Term f
p15 = let' "x" (int 10) (var "x")

seq :: (Apply :<: f, Lambda :<: f) => Term f -> Term f -> Term f
seq t1 t2 = let' "_" t1 t2

----------------------------------------------------------------------------------
-- FlowR

data Privilege = Plus | Minus deriving Show
type Tag = (Name, Privilege)
type Label = [Tag]
data Labels = Labels Label Label
            | Wildcard

data ValueR = ValueR Value Labels
  deriving Show

type EnvironmentR = [(Name,ValueR)]

-- instance Show ValueR where
--     show (ValueR v (Labels r s)) = "ValueR " ++ show v ++ " (" ++ showLabel r ++ "," ++ showLabel s ++ ")"

--type MR m a = M (StateT Labels m) a
--type MR m a = M (StateT Labels (WriterT TraceR m)) a
--type MR m a = (StateT EnvironmentR (StateT Labels (WriterT TraceR m))) a

class Functor f => EvalR f where
  evalR :: (Eval g, EvalR g) => f (Term g) -> M ValueR

instance (EvalR f, EvalR g) => EvalR (f :+: g) where
  evalR (Inl r) = evalR r
  evalR (Inr r) = evalR r

execR :: (Eval f, EvalR f) => Term f -> M ValueR
execR (In t) = evalR t

-- runR :: (Eval f, EvalR f) => Term f -> ValueR
-- runR t = runIdentity $ evalStateT (evalStateT (execR t) [])
-- Wildcard

-- runR :: (Eval f, EvalR f) => Term f -> (ValueR, TraceR)
-- runR t = runWriterT (evalStateT (evalStateT (execR t) []) Wildcard)

-- runpR :: Term (LambdaR :+: Apply :+: Lambda :+: Var :+: Constant :+: Bottom) -> (ValueR, TraceR)
-- runpR = runR

-- runpR :: Term (LambdaR :+: Apply :+: Lambda :+: Var :+: Constant :+: Bottom) -> ValueR
-- runpR = runR

------------------------------------------------------------
-- Delegation

--delegate :: (Monad m, Eval f, Eval g) => f (Term g) -> MR m ValueR
delegate t = eval t >>= \v ->
             get >>= \s ->
             put (liftEnv s) >>
             return (ValueR v (Labels [] []))

liftEnv :: Environment -> EnvironmentR
liftEnv [] = []
liftEnv ((x,v):xs) = (x,(ValueR v (Labels [] []))):(liftEnv xs)

-- instance EvalR Apply where
--     evalR = delegate

instance EvalR Var where
    evalR = delegate

instance EvalR Lambda where
    evalR = delegate

instance EvalR Constant where
    evalR = delegate

instance EvalR Bottom where
    evalR = delegate

------------------------------------------------------------
-- New term

data LambdaR e = LambdaR Name Labels e deriving Functor

instance Eval LambdaR where
    eval (LambdaR x l v) = eval (Lambda x v)

instance EvalR LambdaR where
    evalR (LambdaR x l t) = get >>= \env ->
                            return (ValueR (Closure x t env) l)

lamr :: (LambdaR :<: f) => Name -> Labels -> Term f -> Term f
lamr x l t = inject (LambdaR x l t)

p21 :: (Apply :<: f, Var :<: f, LambdaR :<: f) => Term f
p21 = (app (lamr "x" (Labels [] []) (var "x")) (lamr "x" (Labels [] []) (var "x")))

------------------------------------------------------------
-- Instrumentation

-- instance EvalR Apply where
--     evalR (Apply fun arg) = execR fun >>= \f ->
--                             execR arg >>= \a ->
--                             apply f a


instance EvalR Apply where
    evalR (Apply fun arg) = execR fun >>= \(ValueR (Closure x t env) _) ->
                            execR arg >>= \a ->
                            withEnv ((x,a):env) (execR t)
        where withEnv e f = get >>= \oldEnv ->
                            put e >>
                            f >>= \res ->
                            put oldEnv >>
                            return res


--apply :: Monad m => ValueR -> ValueR -> MR m ValueR
-- apply f@(ValueR (Closure x body env) funLabels) v@(ValueR value valueLabels) =
--     do tell [BeginBlock]
--        tell [Line 0 $ Str $ show f ++ " " ++ show v]

--        callerLabels <- lift get

--        let checkCaller = allow callerLabels funLabels
--            checkArg = case value of
--                         Bot -> True -- Bot arg means no args, 'allow'
--                                     -- checks out
--                         _ -> allow valueLabels funLabels

--        tell [Line 0 $ Call (addDefault callerLabels) (addDefault funLabels) (addDefault valueLabels)]
--        tell [Line 1 $ Allow (addDefault callerLabels) (addDefault funLabels) checkCaller]

--        if not checkCaller
--        then err
--        else do tell [Line 2 $ Allow (addDefault valueLabels) (addDefault funLabels) checkArg]
--                if not checkArg
--                then err
--                else do let previousCaller = callerLabels

--                        lift . put $ funLabels
--                        r@(ValueR rv retLabels) <- withEnv ((x,v):env) (execR body)
--                        -- funLabels might have been modified due to
--                        -- propagation in step 8; use the new ones
--                        -- instead
--                        funLabels' <- lift get
--                        -- then, restore previous caller
--                        lift . put $ previousCaller

--                        tell [Line 5 $ Return r]

--                        let retLabels' = propagate funLabels' retLabels

--                        tell [Line 6 $ Propagate funLabels' retLabels retLabels']
--                        let checkCaller2 = allow retLabels' callerLabels
--                        --let checkCaller2 = True
--                        tell [Line 7 $ Allow (addDefault retLabels') (addDefault callerLabels) checkCaller2]
--                        if not checkCaller2
--                        then err
--                        else do let callerLabels' = propagate retLabels' callerLabels
--                                lift . put $ callerLabels'
--                                tell [Line 8 $ Propagate retLabels' callerLabels callerLabels']

--                                tell [EndBlock]
--                                return (ValueR rv retLabels')
--            where err = do tell [EndBlock]
--                           fail "invalid flow"
--                  withEnv e f = get >>= \oldEnv ->
--                                put e >>
--                                f >>= \res ->
--                                put oldEnv >>
--                                return res


allow :: Labels -> Labels -> Bool
allow Wildcard _ = True
allow _ Wildcard = True
allow (Labels _ s) (Labels r _) = all present s'
    where present (n, _) = case (lookup n r') of
                             Just Plus -> True
                             _ -> False
          s' = addDefaultS s
          r' = addDefaultR r

addDefault :: Labels -> Labels
addDefault (Labels r s) = Labels (addDefaultR r) (addDefaultS s)
addDefault Wildcard = Wildcard

addDefaultS [] = [("default",Plus)]
addDefaultS s = s

addDefaultR r = case (lookup "default" r) of
                  Just _ -> r
                  Nothing -> ("default",Plus):r

propagate :: Labels -> Labels -> Labels
propagate Wildcard b = b
propagate a Wildcard = Wildcard
propagate (Labels _ sa) (Labels rb sb) = Labels rb sb'
    where sb' = let p = filter plusTag sa
                    n = filter minusTag sa
                    r = deleteFirstsBy tagEq sb n
                in unionBy tagEq r p -- order of `r` and `p` matters
                                     -- equality is not symmetric

tagEq :: Tag -> Tag -> Bool
tagEq (n, _) (m, _) = n == m

plusTag :: Tag -> Bool
plusTag (_,Plus) = True
plusTag _ = False

minusTag :: Tag -> Bool
minusTag (_,Minus) = True
minusTag _ = False

------------------------------------------------------------
-- Debugging

data TraceData = Allow Labels Labels Bool
               | Propagate Labels Labels Labels
               | Return ValueR
               | Call Labels Labels Labels
               | Str String

instance Show Labels where
    show (Labels r s) = "(" ++ showLabel r ++ "," ++ showLabel s ++ ")"
    show Wildcard = "*"

showLabel :: Label -> String
showLabel ts = "[" ++ (intercalate " " $ map showTag ts) ++ "]"

showTag :: Tag -> String
showTag ("default",p) = "δ" ++ show p
showTag (n,p) = n ++ show p

showSend :: Labels -> String
showSend Wildcard = "*"
showSend (Labels _ s) = showLabel s

showReceive :: Labels -> String
showReceive Wildcard = "*"
showReceive (Labels r _) = showLabel r

instance Show TraceData where
    show (Allow l1 l2 b) =
        "Allow " ++ showSend l1 ++ " " ++ showReceive l2 ++ " -> " ++ show b
    show (Propagate l1 l2 l3) =
        "Propagate " ++ showSend l1 ++ " " ++ showSend l2 ++ " -> " ++ showSend l3
    show (Return v) = "R := " ++ show v
    show (Call c f v) = "Apply f:" ++ show f
                        ++ " to v:" ++ show v
                        ++ " caller:" ++ show c
    show (Str s) = s

data Trace a = Line Int a
             | BeginBlock
             | EndBlock deriving Show

type TraceR = [Trace TraceData]

------------------------------------------------------------
-- Examples

---------------
-- Patient/Nurse

-- publishRecord = (Lam "x" (Labels [("m",Minus)] []) (Var "x"))
-- getAnonymRecord = (Lam "x" (Labels [] [("m",Minus)]) (App nurseReport Bot))
-- getPatientRecord = (Lam "x" (Labels [] [("m",Plus)]) (App nurseReport Bot))
-- nurseReport = (Lam "x" (Labels [] [("m",Plus)]) (Con 1))
-- ex1 = (App publishRecord (App getAnonymRecord Bot)) -- ok
-- ex2 = (App publishRecord (App getPatientRecord Bot)) -- fail

---------------
-- Declassify

getSecret :: (LambdaR :<: f, Constant :<: f) => Term f
getSecret = (lamr "x" (Labels [] [("s",Plus)]) (int 0))

declassify :: (LambdaR :<: f, Apply :<: f, Bottom :<: f, Constant :<: f) => Term f
declassify = (lamr "x" (Labels [] [("s",Minus)]) (app getSecret bot))

passthrough :: (LambdaR :<: f, Apply :<: f, Bottom :<: f, Constant :<: f) => Term f
passthrough = (lamr "x" (Labels [] []) (app getSecret bot))

p31 :: (LambdaR :<: f, Apply :<: f, Bottom :<: f, Constant :<: f) => Term f
p31 = (app declassify bot) -- ok, return value clean

p32 :: (LambdaR :<: f, Apply :<: f, Bottom :<: f, Constant :<: f) => Term f
p32 = (app passthrough bot) -- return value tainted
