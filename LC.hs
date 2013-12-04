{-# LANGUAGE DeriveDataTypeable #-}

-- i13n stands for 'instrumentation'.  Ideally, all i13n changes to
-- the code should be localized, and could be separately compiled and
-- activated.

import Control.Monad.State
import Control.Monad.Identity
import AOP.Default
import Data.Typeable

import Debug.Trace

type Name      = String
type Address   = Int
type Principal = (Int, Bool) -- i13n
type ProgCounter = [Principal] -- i13n

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
          | Facet Principal Term Term -- i13n

data Value = Error String
           | Bottom
           | Constant Int
           | Boolean Bool
           | Address Int
           | Closure (Value -> M Value)
           | FacetV Principal Value Value -- i13n
   deriving Typeable

-- i13n
createFacetValue :: ProgCounter -> Value -> Value -> Value
createFacetValue [] vH vL                 = vH
createFacetValue (k@(_,True):rest)  vH vL = FacetV k (createFacetValue rest vH vL) vL
createFacetValue (k@(_,False):rest) vH vL = FacetV k vL (createFacetValue rest vH vL)

-- data TermA = ...
-- data TermR = ...
-- data TermF = ...

-- class InterpC t where
--   interpC :: Term -> Environment -> MValue

instance Eq Value where
  Error s1 == Error s2               = s1 == s2
  Bottom == Bottom                   = True
  Constant i1 == Constant i2         = i1 == i2
  Boolean b1 == Boolean b2 = b1 == b2
  Address i1 == Address i2           = i1 == i2
  Closure f == Closure g             = False
  FacetV p v1 v2 == FacetV q w1 w2   = p == q && v1 == w1 && v2 == w2 -- i13n

instance Show Value where
  show (Error s)        = "<Error> " ++ show s
  show Bottom           = "<bottom>"
  show (Constant i)     = show i
  show (Boolean b) = show b
  show (Address i)      = "<address> " ++ show i
  show (Closure f)      = "<function>"
  show (FacetV p v1 v2) = "(FacetV: " ++ show p ++ ", " ++ show v1 ++ ", " ++ show v2 ++ ")" -- i13n

type Environment = [(Name, Value)]
type Store = [(Address, Value)]

-- i13n
type M a = AOT (StateT ProgCounter (StateT Store Identity)) a

-- i13n
runM :: M Value -> ProgCounter -> Store -> ((Value, ProgCounter), Store)
runM m pc s = runIdentity (runStateT (runStateT (runAOT prog) pc) s)
 where prog = do deploy (aspect (pcCall goIf) goIfAdv) -- i13n
                 deploy (aspect (pcCall goRef) goRefAdv) -- i13n
                 m

goIf :: (Environment, Term) -> M Value
goIf (e, (If cond thn els)) =
  do b <- interp cond e
     case b of
       Bottom -> return Bottom
       (Boolean b) -> if b then interp thn e
                                else interp els e

-- i13n
goIfAdv proceed args@(e, (If cond thn els)) =
  do b <- interp cond e
     case b of
      (FacetV p (Boolean vH) (Boolean vL)) ->
        do eH <- interp (If (Bol vH) thn els) e
           eL <- interp (If (Bol vL) thn els) e
           return (FacetV p eH eL)
      otherwise -> proceed args

goRef :: (Environment, Term) -> M Value
goRef (e,(Ref t)) =
  do v <- interp t e
     store <- lift $ lift $ get
     let addr = length store
     (lift . lift . put) ((addr,v):store)
     return (Address addr)

-- i13n
goRefAdv proceed args@(e, (Ref t)) =
  do result@(Address addr) <- proceed args
     progCounter <- get
     store <- lift $ lift $ get
     let v = storeLookup addr store
     (lift . lift . put) (storeReplace
                           addr
                           (createFacetValue progCounter v Bottom)
                           store)
     return result

interp :: Term -> Environment -> M Value
interp Bot e         = return Bottom
interp (Con i) e     = return (Constant i)
interp (Bol b) e = return (Boolean b)
interp (Var x) e     = return (envLookup x e)
interp (Lam x v) e   = return (Closure (\a -> interp v ((x,a):e)))

-- i13n
-- should yield terms rather than values, see rule F-IF-SPLIT
interp (Facet p t1 t2) e =
  do vH <- interp t1 e
     vL <- interp t2 e
     return (FacetV p vH vL)

interp (App t u) e =
  do f <- interp t e
     a <- interp u e
     apply f a

-- i13n
interp expr@(Ref t) e = goRef # (e, expr)

interp (Deref t) e =
  do v <- interp t e
     deref v

interp (Assign l r) e =
  do lv <- interp l e
     rv <- interp r e
     assign lv rv

-- i13n
interp expr@(If cond thn els) e = goIf # (e, expr)

-- desugaring
interp (Let id namedExpr body) e = interp (App (Lam id body) namedExpr) e
interp (Seq left right) e = interp (Let "freevar" left right) e

envLookup :: Name -> Environment -> Value
envLookup x [] = (Error ("unbound " ++ show x))
envLookup x ((y,b):e) = if x == y then b else envLookup x e

storeLookup :: Address -> Store -> Value
storeLookup a [] = (Error ("not in store " ++ show a))
storeLookup a ((b,v):s) = if a == b then v else storeLookup a s

storeReplace :: Address -> Value -> Store -> Store
storeReplace a v [] = []
storeReplace a v ((b,w):s) = if a == b then ((a,v):s) else storeReplace a v s

apply :: Value -> Value -> M Value
apply (Closure f) a = f a
apply Bottom _      = return Bottom
apply _ _           = return (Error "apply: not a closure or bottom")

deref :: Value -> M Value
deref Bottom      = return Bottom
deref (Address a) = do store <- lift $ lift $ get
                       return (storeLookup a store)
deref _           = return (Error "deref: not an address or bottom")

assign :: Value -> Value -> M Value
assign Bottom _      = return Bottom
assign (Address a) v = do store <- lift $ lift $ get
                          (lift . lift . put) (storeReplace a v store)
                          return v

assign _ _ = return (Error "assign: not an address or bottom")

-- use implicit parameters??

test :: Term -> Environment -> ProgCounter -> Store -> String
test t env pc store = show (runM (interp t env) pc store)

testDefault t = test t [] [] []

-- i13n
testWithPC :: Term -> ProgCounter -> String
testWithPC t pc = show (runM (interp t []) pc [])

-- i13n
runWithPC t pc = runM (interp t []) pc []

-- Tests, we skip them for now!

term0 = (App (Lam "x" (Var "x")) (Con 10))
termBot = (App (Bot) (Con 1))
termStore = (Deref (App (Lam "x" (App (Lam "y" (Var "x"))
                                      (Assign (Var "x") (Con 2))))
                        (Ref (Con 1))))

termLet = (Let "x" (Con 1) (Var "x"))
termSeq = (Let "x" (Ref (Con 1))
           (Seq
            (Assign (Var "x") (Con 2))
            ((Deref (Var "x")))))


-- assert facetTest1 == (Facet (1,True) (Con 42) (Con 24))
facetTest1 = (If (Facet (1,True) (Bol True) (Bol False)) (Con 42) (Con 24))

-- assert facetTest1 ==
facetTest2 = (Ref (Con 843))

fentonTest = (Let "x" (Ref (Facet (1,True) (Bol True) (Bot)))
              (Let "y" (Ref (Bol True))
               (Let "z" (Ref (Bol True))
                (Seq
                 (If (Deref (Var "x"))
                  (Assign (Var "y") (Bol False))
                  (Bot))
                 (If (Deref (Var "y"))
                  (Assign (Var "z") (Bol False))
                  (Bot))))))

-- let x = ref (<1 ? true : ⟂>) in (
--   let y = ref true in (
--     let z = ref true in (
--       if !x then y := false;  # y = <1 ? false : true>
--       if !y then z := false;  # z = <1 ? true : false>
--         !z)))
