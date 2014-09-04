{-# LANGUAGE FlexibleContexts #-}

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
          | Facet Principal Term Term
          deriving (Show, Eq)

data Value = Bottom
           | Constant Int
           | Address Int
           | Closure Name Term Environment
           | ValueR Value Labels
           | FacetV Principal Value Value
           deriving (Show, Eq)

type Environment = [(Name, Value)]
type Store = [(Address, Value)]

data EvalState = EvalState { env :: Environment
                           , store :: Store }
               | EvalStateR { env :: Environment
                            , store :: Store
                            , labels :: Labels }
               | EvalStateF { env :: Environment
                            , store :: Store
                            , progCounter :: ProgCounter }
               deriving Show

-- Boilerplate modification functions
inEnv :: (Environment -> Environment) -> EvalState -> EvalState
inEnv f s = s { env = f (env s) }

inStore :: (Store -> Store) -> EvalState -> EvalState
inStore f s = s { store = f (store s) }

puts :: MonadState s m => ((a -> b) -> s -> s) -> b -> m ()
puts f v = modify (f (\_ -> v))

type M = State EvalState

eval :: Term -> M Value
eval Bot = return Bottom

eval (Con n) = return $ Constant n

eval (Var x) =
  gets env >>= \e ->
  case lookup x e of
    Just v -> return v
    Nothing -> fail $ "Unbound variable: " ++ x

eval (Lam x body) =
  gets env >>= \e ->
  return (Closure x body e)

eval (App f v) =
  eval f >>= \f ->
  eval v >>= \v ->
  apply f v

eval (LamR x l body) =
  eval (Lam x body) >>= \c ->
  return (ValueR c l)

-- FIXME: should yield terms, not values
eval (Facet p t1 t2) =
  eval t1 >>= \h ->
  eval t2 >>= \l ->
  return (FacetV p h l)

eval (Ref t) =
  eval t >>= \v ->
  gets store >>= \s ->
  let addr = length s in
  puts inStore ((addr,v) : s) >>
  return (Address addr)

eval (Deref t) =
  eval t >>= \v ->
  case v of
    Bottom -> return Bottom
    (Address a) -> gets store >>= \s ->
      case lookup a s of
        Just v -> return v
        Nothing -> fail $ "Not in store: " ++ show a

eval (Assign l r) =
  eval l >>= \lv ->
  eval r >>= \rv ->
  case lv of
    Bottom -> return Bottom
    (Address a) -> modify (inStore (replace a rv)) >>
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

apply (FacetV p vH vL) v2 =
  gets progCounter >>= \pc ->
  if (p,True) `elem` pc
  then apply vH v2
  else
    if (p,False) `elem` pc
    then apply vL v2
    else puts inProgCounter ((p,True):pc) >>
         apply vH v2 >>= \vH' ->
         puts inProgCounter ((p,False):pc) >>
         apply vL v2 >>= \vL' ->
         puts inProgCounter pc >>
         return (createFacetValue [(p,True)] vH' vL')

withEnv :: (MonadState EvalState m) => Environment -> m b -> m b
withEnv e f =
    gets env >>= \old ->
    puts inEnv e >>
    f >>= \r ->
    puts inEnv old >>
    return r

interp :: Term -> (Value, EvalState)
interp t = runState (eval t) (EvalState { env = []
                                        , store = [] })

term0 :: Term
term0 = (App (Lam "x" (Var "x")) (Con 10))

---------------------------------------------------------
-- FlowR instrumentation

type MR = WriterT (TraceR Value) M

inLabels :: (Labels -> Labels) -> EvalState -> EvalState
inLabels f s = s { labels = f (labels s) }

evalR :: Term -> MR Value
-- override default behavior for App
evalR (App f v) = evalR f >>= \f ->
                  evalR v >>= \v ->
                  applyR f v

-- fallback for other terms
evalR t = lift (eval t)

interpR :: Term -> ((Value, (TraceR Value)), EvalState)
interpR t = let a = runWriterT (evalR t)
                b = runState a baseState
                baseState = EvalStateR { env = []
                                       , store = []
                                       , labels = Wildcard }
            in b

applyR :: Value -> Value -> MR Value
applyR f@(ValueR c@(Closure x body env) funLabels) value =
    do tell [BeginBlock]
       tell [Line 0 $ Str $ show f ++ " " ++ show value]

       callerLabels <- gets labels
       let valueLabels = extractLabels value

       tell [Line 0 $ Call (addDefault callerLabels) (addDefault funLabels) (addDefault valueLabels)]

       allowCheck 1 callerLabels funLabels

       when (value /= Bottom)   -- Bot arg means no args, skip check
         (allowCheck 2 valueLabels funLabels)

       puts inLabels funLabels
       r <- withEnv ((x, value) : env) (evalR body)
       -- funLabels might have been modified due to
       -- propagation in step 8; use the new ones
       -- instead
       funLabels' <- gets labels
       -- then, restore previous caller
       puts inLabels callerLabels

       tell [Line 5 $ Return r]

       let retLabels = extractLabels r
           retLabels' = propagate funLabels' retLabels

       tell [Line 6 $ Propagate funLabels' retLabels retLabels']

       allowCheck 7 retLabels' callerLabels

       let callerLabels' = propagate retLabels' callerLabels
       puts inLabels callerLabels'
       tell [Line 8 $ Propagate retLabels' callerLabels callerLabels']

       tell [EndBlock]
       return (ValueR (extractValue r) retLabels')

-- fallback to basic apply
applyR f v = lift (apply f v)

-- Return empty labels for a plain value
extractLabels :: Value -> Labels
extractLabels (ValueR _ l) = l
extractLabels _ = Labels [] []

-- Return empty labels for a plain value
extractValue :: Value -> Value
extractValue (ValueR v _) = v
extractValue v = v

-- running
prettyTest :: Term -> IO ()
prettyTest t = do printTrace trace (-2)
                  print value
    where result = interpR t
          value = fst (fst result)
          trace = snd (fst result)

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

type Principal = Int
type Branch    = (Principal,Bool)

type ProgCounter = [Branch] -- i13n

inProgCounter :: (ProgCounter -> ProgCounter) -> EvalState -> EvalState
inProgCounter f s = s { progCounter = f (progCounter s) }

evalF :: Term -> M Value
evalF (Ref t) =
  evalF t >>= \v ->
  gets store >>= \s ->
  let addr = length s in
  gets progCounter >>= \pc ->
  let fv = createFacetValue pc v Bottom in
  puts inStore ((addr,fv) : s) >>
  return (Address addr)

evalF t = eval t

interpF :: Term -> (Value, EvalState)
interpF t = runState (evalF t) EvalStateF { env = []
                                          , store = []
                                          , progCounter = [] }

createFacetValue :: ProgCounter -> Value -> Value -> Value
createFacetValue [] vH vL = vH
createFacetValue ((k,True):rest)  vH vL = FacetV k (createFacetValue rest vH vL) vL
createFacetValue ((k,False):rest) vH vL = FacetV k vL (createFacetValue rest vH vL)

-- Examples
facetTest1 = (If (Facet 1 (Bol True) (Bol False)) (Con 42) (Con 24))

fentonTest = (Let "x" (Ref (Facet 1 (Bol True) Bot))
              (Let "y" (Ref (Bol True))
               (Let "z" (Ref (Bol True))
                (Seq
                 (Seq
                  (If (Deref (Var "x"))
                   (Assign (Var "y") (Bol False))
                   Bot)
                  (If (Deref (Var "y"))
                   (Assign (Var "z") (Bol False))
                   Bot))
                 (Deref (Var "z"))))))
