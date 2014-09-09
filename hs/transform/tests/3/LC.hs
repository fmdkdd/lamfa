import Control.Monad.State.Lazy
import Control.Monad.Writer

import FlowR

type Name = String

opendata Term = Bot
              | Con Int
              | Var Name
              | Lam Name Term
              | App Term Term
              deriving (Show, Eq)

opendata Value = Bottom
               | Constant Int
               | Closure Name Term Environment
               deriving (Show, Eq)

type Environment = [(Name, Value)]

type M = State Environment

eval :: Term -> M Value
eval Bot = return Bottom

eval (Con n) = return $ Constant n

eval (Var x) =
  get >>= \env ->
  case lookup x env of
    Just v -> return v
    Nothing -> fail $ "Unbound variable: " ++ x

eval (Lam x body) =
    get >>= \env ->
    return $ Closure x body env

eval (App f v) =
    eval f >>= \f ->
    eval v >>= \v ->
    apply f v


apply :: Value -> Value -> M Value
apply Bottom _ = return Bottom
apply (Closure x body env) v = withEnv ((x,v) : env) (eval body)

withEnv :: Environment -> M Value -> M Value
withEnv env f =
  get >>= \oldEnv ->
  put env >>
  f >>= \r ->
  put oldEnv >>
  return r

interp :: Term -> (Value, Environment)
interp t = runState (eval t) []

term0 :: Term
term0 = (App (Lam "x" (Var "x")) (Con 10))

------------------------------------------------------------
-- FlowR instrumentation

extenddata Term where
  LamR Name Labels Term

extenddata Value where
  ValueR Value Labels

extend eval where
  eval (LamR x l body) =
    eval (Lam x body) >>= \c ->
    return $ ValueR c l

type MR = WriterT (TraceR Value) (StateT Labels M)

evalR :: Term -> MR Value
-- override default behavior for App
evalR (App f v) = evalR f >>= \f ->
                  evalR v >>= \v ->
                  applyR f v

-- fallback for other terms
evalR t = lift (lift (eval t))

interpR :: Term -> (((Value, (TraceR Value)), Labels), Environment)
interpR t = runState (runStateT (runWriterT (evalR t)) Wildcard) []

applyR :: Value -> Value -> MR Value
applyR f@(ValueR c@(Closure x body env) funLabels) value =
    do tell [BeginBlock]
       tell [Line 0 $ Str $ show f ++ " " ++ show value]

       callerLabels <- get
       let valueLabels = extractLabels value

       tell [Line 0 $ Call (addDefault callerLabels)
             (addDefault funLabels)
             (addDefault valueLabels)]

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
    (lift (lift get)) >>= \oldEnv ->
    (lift . lift . put) env >>
    f >>= \r ->
    (lift . lift . put) oldEnv >>
    return r

-- running
prettyTest :: Term -> IO ()
prettyTest t = do printTrace trace (-2)
                  print value
    where result = interpR t
          value = fst (fst (fst result))
          trace = snd (fst (fst result))

--- examples
term1 :: Term
term1 = (App (LamR "x" (Labels [] []) (Var "x")) (Con 10))

termBase :: Term
termBase = (LamR "x" (Labels [("m",Minus)] [("s",Plus)]) (Var "x"))

termBot = (App (LamR "x" (Labels [] []) (Con 1)) Bot)

-- Patient/Nurse with caller check
publishRecord' = (LamR "x" (Labels [("m",Minus)] []) (Var "x"))
getAnonymRecord' = (LamR "x"
                    (Labels [("m",Plus)] [("m",Minus)])
                    (App nurseReport' Bot))
getPatientRecord' = (LamR "x"
                     (Labels [("m",Plus)] [("m",Plus)])
                     (App nurseReport' Bot))
nurseReport' = (LamR "x" (Labels [("m",Plus)] [("m",Plus)]) (Con 1))
ex1' = (App publishRecord' (App getAnonymRecord' Bot)) -- ok
ex2' = (App publishRecord' (App getPatientRecord' Bot)) -- fail
