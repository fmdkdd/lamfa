import Control.Monad.State.Lazy
import Control.Monad.Writer

import FlowR

type Name = String
type Address = Int

data Term = Bot
          | Con Int
          | Bol Bool
          | Var Name
          | Lam Name Term
          | App Term Term
          | Ref Term
          | Deref Term
          | Assign Term Term
          | If Term Term Term
          | Let Name Term Term
          | Seq Term Term
          | LamR Name Labels Term
          deriving (Show, Eq)

data Value = Bottom
           | Constant Int
           | Address Int
           | Closure Name Term Environment
           | ValueR Value Labels
           deriving (Show, Eq)

type Environment = [(Name, Value)]
type Store = [(Address, Value)]

type M = StateT Store (State Environment)

eval :: Term -> M Value
eval Bot = return Bottom

eval (Con n) = return $ Constant n

eval (Var x) =
  (lift get) >>= \env ->
  case lookup x env of
    Just v -> return v
    Nothing -> fail $ "Unbound variable: " ++ x

eval (Lam x body) =
  (lift get) >>= \env ->
  return (Closure x body env)

eval (App f v) =
  eval f >>= \f ->
  eval v >>= \v ->
  apply f v

eval (LamR x l body) =
  eval (Lam x body) >>= \c ->
  return (ValueR c l)

eval (Ref t) =
  eval t >>= \v ->
  get >>= \store ->
  let addr = length store in
  put ((addr,v) : store) >>
  return (Address addr)

eval (Deref t) =
  eval t >>= \v ->
  case v of
    Bottom -> return Bottom
    (Address a) -> get >>= \store ->
      case lookup a store of
        Just v -> return v
        Nothing -> fail $ "Not in store: " ++ show a

eval (Assign l r) =
  eval l >>= \lv ->
  eval r >>= \rv ->
  case lv of
    Bottom -> return Bottom
    (Address a) -> modify (replace a rv) >>
                   return rv

-- desugaring
eval (Let x t body) = eval (App (Lam x body) t)
eval (Seq left right) = eval (Let "freevar" left right)
eval (If c thn els) = eval (App
                            (App
                             (App c (Lam "d" thn))
                             (Lam "d" els))
                            (Lam "x" (Var "x")))
eval (Bol True) = eval (Lam "x" (Lam "y" (Var "x")))
eval (Bol False) = eval (Lam "x" (Lam "y" (Var "y")))

replace :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replace _ _ [] = []
replace k newv ((k',v):s) = if k == k'
                            then (k,newv) : s
                            else (k,v) : replace k newv s

apply :: Value -> Value -> M Value
apply Bottom _ = return Bottom
apply (Closure x body env) v = withEnv ((x,v) : env) (eval body)

withEnv :: Environment -> M Value -> M Value
withEnv env f =
    (lift get) >>= \oldEnv ->
    (lift . put) env >>
    f >>= \r ->
    (lift . put) oldEnv >>
    return r

interp :: Term -> ((Value, Store), Environment)
interp t = let a = runStateT (eval t) []
               b = runState a []
           in b

term0 :: Term
term0 = (App (Lam "x" (Var "x")) (Con 10))

---------------------------------------------------------
-- FlowR instrumentation

type MR = WriterT (TraceR Value) (StateT Labels M)

evalR :: Term -> MR Value
-- override default behavior for App
evalR (App f v) = evalR f >>= \f ->
                  evalR v >>= \v ->
                  applyR f v

-- fallback for other terms
evalR t = lift (lift (eval t))

interpR :: Term -> ((((Value, (TraceR Value)), Labels), Store), Environment)
interpR t = let a = runWriterT (evalR t) -- WriterT TraceR
                b = runStateT a Wildcard -- StateT Labels
                c = runStateT b []       -- StateT Store
                d = runState c []       -- State Environment
            in d

applyR :: Value -> Value -> MR Value
applyR f@(ValueR c@(Closure x body env) funLabels) value =
    do tell [BeginBlock]
       tell [Line 0 $ Str $ show f ++ " " ++ show value]

       callerLabels <- get
       let valueLabels = extractLabels value

       tell [Line 0 $ Call (addDefault callerLabels) (addDefault funLabels) (addDefault valueLabels)]

       allowCheck 1 callerLabels funLabels

       when (value /= Bottom)   -- Bot arg means no args, skip check
         (allowCheck 2 valueLabels funLabels)

       put funLabels
       r <- withEnvR ((x, value) : env) (evalR body)
       -- funLabels might have been modified due to
       -- propagation in step 8; use the new ones
       -- instead
       funLabels' <- get
       -- then, restore previous caller
       put callerLabels

       tell [Line 5 $ Return r]

       let retLabels = extractLabels r
           retLabels' = propagate funLabels' retLabels

       tell [Line 6 $ Propagate funLabels' retLabels retLabels']

       allowCheck 7 retLabels' callerLabels

       let callerLabels' = propagate retLabels' callerLabels
       put callerLabels'
       tell [Line 8 $ Propagate retLabels' callerLabels callerLabels']

       tell [EndBlock]
       return (ValueR (extractValue r) retLabels')

-- fallback to basic apply
applyR f v = lift (lift (apply f v))

-- Return empty labels for a plain value
extractLabels :: Value -> Labels
extractLabels (ValueR _ l) = l
extractLabels _ = Labels [] []

-- Return empty labels for a plain value
extractValue :: Value -> Value
extractValue (ValueR v _) = v
extractValue v = v

withEnvR :: Environment -> MR Value -> MR Value
withEnvR env f =
    (lift (lift (lift get))) >>= \oldEnv ->
    (lift . lift . lift . put) env >>
    f >>= \r ->
    (lift . lift . lift . put) oldEnv >>
    return r

-- running
prettyTest :: Term -> IO ()
prettyTest t = do printTrace trace (-2)
                  print value
    where result = interpR t
          value = fst (fst (fst (fst result)))
          trace = snd (fst (fst (fst result)))

--- examples
term1 :: Term
term1 = (App (LamR "x" (Labels [] []) (Var "x")) (Con 10))

termBase :: Term
termBase = (LamR "x" (Labels [("m",Minus)] [("s",Plus)]) (Var "x"))

termBot = (App (LamR "x" (Labels [] []) (Con 1)) Bot)

-- Patient/Nurse with caller check
publishRecord' = (LamR "x" (Labels [("m",Minus)] []) (Var "x"))
getAnonymRecord' = (LamR "x" (Labels [("m",Plus)] [("m",Minus)]) (App nurseReport' Bot))
getPatientRecord' = (LamR "x" (Labels [("m",Plus)] [("m",Plus)]) (App nurseReport' Bot))
nurseReport' = (LamR "x" (Labels [("m",Plus)] [("m",Plus)]) (Con 1))
ex1' = (App publishRecord' (App getAnonymRecord' Bot)) -- ok
ex2' = (App publishRecord' (App getPatientRecord' Bot)) -- fail

--------------------------------------------------------
-- Language with references and sugar (let, seq, if)

termStore = (Deref (App (Lam "x" (App (Lam "y" (Var "x"))
                                      (Assign (Var "x") (Con 2))))
                        (Ref (Con 1))))

termLet = (Let "x" (Con 1) (Var "x"))
termSeq = (Let "x" (Ref (Con 1))
           (Seq
            (Assign (Var "x") (Con 2))
            ((Deref (Var "x")))))

---------------------------------------------------------
-- Facets instrumentation
