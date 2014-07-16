{-# LANGUAGE PackageImports #-}

import Control.Monad.State.Lazy
import Control.Monad.Writer

import Data.List

type Name = String

opendata Term = Bot
              | Con Int
              | Var Name
              | Lam Name Term
              | App Term Term
  deriving Show

opendata Value = Bottom
               | Constant Int
               | Closure Name Term Environment
  deriving Show

type Environment = [(Name, Value)]

type M = State Environment

eval :: Term -> M Value
eval Bot = return Bottom

eval (Con n) =
    return $ Constant n

eval (Var x) = get >>= \env ->
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
apply (Closure x body env) v = withEnv ((x,v):env) (eval body)

withEnv :: Environment -> M Value -> M Value
withEnv env f = get >>= \oldEnv ->
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
  eval (LamR x l body) = eval (Lam x body) >>= \c ->
                         return $ ValueR c l

withEnvR :: Environment -> MR Value -> MR Value
withEnvR env f =
    (lift (lift get)) >>= \oldEnv ->
    (lift . lift . put) env >>
    f >>= \r ->
    (lift . lift . put) oldEnv >>
    return r

data Privilege = Plus | Minus

instance Show Privilege where
    show Plus = "+"
    show Minus = "-"

type Tag = (Name, Privilege)
type Label = [Tag]
data Labels = Labels Label Label
            | Wildcard

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

data TraceData = Allow Labels Labels Bool
               | Propagate Labels Labels Labels
               | Return Value
               | Call Labels Labels Labels
               | Str String

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
             | EndBlock
  deriving Show

type TraceR = [Trace TraceData]

type MR = WriterT TraceR (StateT Labels M)

evalR :: Term -> MR Value
evalR (App f v) = evalR f >>= \f ->
                  evalR v >>= \v ->
                  applyR f v
evalR t = lift (lift (eval t))

interpR :: Term -> (((Value, TraceR), Labels), Environment)
interpR t = runState (runStateT (runWriterT (evalR t)) Wildcard) []

applyR :: Value -> Value -> MR Value
applyR f@(ValueR c@(Closure x body env) funLabels) value =
    do tell [BeginBlock]
       tell [Line 0 $ Str $ show f ++ " " ++ show value]

       callerLabels <- get

       let valueLabels = case value of
                           (ValueR _ lv) -> lv
                           _ -> (Labels [] [])
           checkArg = case value of
                        Bottom -> False -- Bot arg means no args, 'allow'
                                        -- checks out
                        _ -> True

       tell [Line 0 $ Call (addDefault callerLabels) (addDefault funLabels) (addDefault valueLabels)]

       allowCheck 1 callerLabels funLabels

       when checkArg (allowCheck 2 valueLabels funLabels)

       let previousCaller = callerLabels

       put funLabels
       r <- withEnvR ((x, value) : env) (evalR body)
       -- funLabels might have been modified due to
       -- propagation in step 8; use the new ones
       -- instead
       funLabels' <- get
       -- then, restore previous caller
       put previousCaller

       tell [Line 5 $ Return r]

       let retLabels = case r of
                         (ValueR _ v) -> v
                         _ -> Labels [] []
           rv = case r of
                  (ValueR v _) -> v
                  _ -> r
           retLabels' = propagate funLabels' retLabels

       tell [Line 6 $ Propagate funLabels' retLabels retLabels']

       allowCheck 7 retLabels' callerLabels

       let callerLabels' = propagate retLabels' callerLabels
       put callerLabels'
       tell [Line 8 $ Propagate retLabels' callerLabels callerLabels']

       tell [EndBlock]
       return (ValueR rv retLabels')

applyR f v = lift (lift (apply f v))

liftR :: (Monad m) => m Value -> m Value
liftR mx = mx >>= \x ->
           case x of
             (ValueR _ _) -> return x
             v -> return (ValueR x (Labels [] []))

allowCheck :: (MonadWriter TraceR m) => Int -> Labels -> Labels -> m ()
allowCheck step l1 l2 = tell [Line step $ Allow (addDefault l1) (addDefault l2) allowed] >>
                        when (not allowed) (fail "invalid flow")
    where allowed = allow l1 l2

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

addDefaultS :: [Tag] -> [Tag]
addDefaultS [] = [("default",Plus)]
addDefaultS s = s

addDefaultR :: [Tag] -> [Tag]
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

-- running
prettyTest :: Term -> IO ()
prettyTest t = do printTrace trace (-2)
                  print value
    where result = interpR t
          value = fst (fst (fst result))
          trace = snd (fst (fst result))

printTrace :: TraceR -> Int -> IO ()
printTrace [] _ = return ()
printTrace ((Line i s):ts) n = do putStrLn $ (nspaces n) ++ show i ++ ". " ++ show s
                                  printTrace ts n
printTrace ((BeginBlock):ts) n = do printTrace ts (n + 2)
printTrace ((EndBlock):ts) n = do printTrace ts (n - 2)

nspaces :: Int -> String
nspaces n = concat $ replicate n " "

--- examples
termBase :: Term
termBase = (LamR "x" (Labels [("m",Minus)] [("s",Plus)]) (Var "x"))

-- Patient/Nurse with caller check
publishRecord' = (LamR "x" (Labels [("m",Minus)] []) (Var "x"))
getAnonymRecord' = (LamR "x" (Labels [("m",Plus)] [("m",Minus)]) (App nurseReport' Bot))
getPatientRecord' = (LamR "x" (Labels [("m",Plus)] [("m",Plus)]) (App nurseReport' Bot))
nurseReport' = (LamR "x" (Labels [("m",Plus)] [("m",Plus)]) (Con 1))
ex1' = (App publishRecord' (App getAnonymRecord' Bot)) -- ok
ex2' = (App publishRecord' (App getPatientRecord' Bot)) -- fail
