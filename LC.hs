{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad.State
import Control.Monad.Identity
import AOP.Default
import Data.Typeable

import Debug.Trace

type Name      = String
type Address   = Int
type Principal = (Int, Bool)
type ProgCounter = [Principal]
 
data Term = Bot
          | Con Int
          | Boolean Bool
          | Var Name
          | Lam Name Term
          | App Term Term
          | Ref Term
          | Deref Term
          | Assign Term Term
          | If Term Term Term
          | Let Name Term Term
          | Facet Principal Term Term

data Value = Error String
           | Bottom
           | Constant Int
           | BoolConstant Bool
           | Address Int
           | Closure (Value -> M Value)
           | FacetV Principal Value Value
   deriving Typeable


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
  BoolConstant b1 == BoolConstant b2 = b1 == b2
  Address i1 == Address i2           = i1 == i2
  Closure f == Closure g             = False
  FacetV p v1 v2 == FacetV q w1 w2   = p == q && v1 == w1 && v2 == w2

instance Show Value where
  show (Error s)        = "<Error> " ++ show s
  show Bottom           = "<bottom>"
  show (Constant i)     = show i
  show (BoolConstant b) = show b
  show (Address i)      = "<address> " ++ show i
  show (Closure f)      = "<function>"
  show (FacetV p v1 v2) = "(FacetV: " ++ show p ++ ", " ++ show v1 ++ ", " ++ show v2 ++ ")"
  
type Environment = [(Name, Value)]
type Store = [(Address, Value)]

type M a = AOT (StateT ProgCounter (StateT Store Identity)) a

runM :: M Value -> ProgCounter -> Store -> ((Value, ProgCounter), Store)
runM m pc s = runIdentity (runStateT (runStateT (runAOT prog) pc) s)
 where prog = do deploy (aspect (pcCall goIf) goIfAdv)
                 deploy (aspect (pcCall goRef) goRefAdv)                 
                 m

goIf :: (Environment, Term) -> M Value
goIf (e, (If cond thn els)) =
  do b <- interp cond e
     case b of
       Bottom -> return Bottom
       (BoolConstant b) -> if b then interp thn e
                                else interp els e

goIfAdv proceed args@(e, (If cond thn els)) =
  do b <- interp cond e
     case b of
      (FacetV p (BoolConstant vH) (BoolConstant vL)) ->
        do eH <- interp (If (Boolean vH) thn els) e
           eL <- interp (If (Boolean vL) thn els) e
           return (FacetV p eH eL)
      otherwise -> proceed args

goRef :: (Environment, Term) -> M Value
goRef (e,(Ref t)) =
  do v <- interp t e
     store <- lift $ lift $ get
     let addr = length store
     (lift . lift . put) ((addr,v):store)
     return (Address addr)

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
interp (Boolean b) e = return (BoolConstant b)
interp (Var x) e     = return (envLookup x e)
interp (Lam x v) e   = return (Closure (\a -> interp v ((x,a):e)))

-- should yield terms rather than values, see rule F-IF-SPLIT
interp (Facet p t1 t2) e =
  do vH <- interp t1 e
     vL <- interp t2 e
     return (FacetV p vH vL)
          
interp (App t u) e = 
  do f <- interp t e
     a <- interp u e
     apply f a
                          
interp expr@(Ref t) e = goRef # (e, expr)

interp (Deref t) e =
  do v <- interp t e
     deref v

interp (Assign l r) e =
  do lv <- interp l e
     rv <- interp r e
     assign lv rv

interp expr@(If cond thn els) e = goIf # (e, expr)

interp (Let id namedExpr body) e = interp (App (Lam id body) namedExpr) e

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

testWithPC :: Term -> ProgCounter -> String
testWithPC t pc = show (runM (interp t []) pc [])

runWithPC t pc = runM (interp t []) pc []

-- Tests, we skip them for now!

term0 = (App (Lam "x" (Var "x")) (Con 10))
termBot = (App (Bot) (Con 1))
termStore = (Deref (App (Lam "x" (App (Lam "y" (Var "x"))
                                      (Assign (Var "x") (Con 2))))
                        (Ref (Con 1))))

-- assert facetTest1 == (Facet (1,True) (Con 42) (Con 24))
facetTest1 = (If (Facet (1,True) (Boolean True) (Boolean False)) (Con 42) (Con 24))

-- assert facetTest1 ==
facetTest2 = (Ref (Con 843))


  

