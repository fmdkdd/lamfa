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

type Principal = (Int, Bool) -- i13n

-- i13n
createFacetValue :: ProgCounter -> Value -> Value -> Value
createFacetValue [] vH vL                 = vH
createFacetValue (k@(_,True):rest)  vH vL = FacetV k (createFacetValue rest vH vL) vL
createFacetValue (k@(_,False):rest) vH vL = FacetV k vL (createFacetValue rest vH vL)

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
type ProgCounter = [Principal] -- i13n

-- i13n
type M a = AOT (StateT ProgCounter (StateT Store Identity)) a

-- i13n
runM :: M Value -> ProgCounter -> Store -> ((Value, ProgCounter), Store)
runM m pc s = runIdentity (runStateT (runStateT (runAOT prog) pc) s)
 where prog = do deploy (aspect (pcCall goIf) goIfAdv)         -- i13n
                 deploy (aspect (pcCall goRef) goRefAdv)       -- i13n
                 deploy (aspect (pcCall goDeref) goDerefAdv)   -- i13n
                 deploy (aspect (pcCall goAssign) goAssignAdv) -- i13n
                 m

interp :: Term -> Environment -> M Value
interp Bot e         = return Bottom
interp (Con i) e     = return (Constant i)
interp (Bol b) e     = return (Boolean b)
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

interp expr@(Ref t) e = goRef # (e, expr) -- i13n

interp (Deref t) e =
  do v <- interp t e
     deref v

interp (Assign l r) e =
  do lv <- interp l e
     rv <- interp r e
     assign lv rv

interp expr@(If cond thn els) e = goIf # (e, expr) -- i13n

-- desugaring
interp (Let id namedExpr body) e = interp (App (Lam id body) namedExpr) e
interp (Seq left right) e        = interp (Let "freevar" left right) e


-- "open" rules and advices
-- implicit i13n

------------------------------ REF
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
     let fv = (createFacetValue progCounter v Bottom)
     (lift . lift . put) (storeReplace addr fv store)
     return result


------------------------------ IF
goIf :: (Environment, Term) -> M Value
goIf (e, (If cond thn els)) =
  do b <- interp cond e
     case b of
       Bottom -> return Bottom
       (Boolean b) -> if b then interp thn e
                      else interp els e

goIfAdv proceed args@(e, (If cond thn els)) =
  do b <- interp cond e
     case b of
      (FacetV p (Boolean vH) (Boolean vL)) ->
        do eH <- interp (If (Bol vH) thn els) e
           eL <- interp (If (Bol vL) thn els) e
           return (FacetV p eH eL)

      (FacetV p (Boolean vH) Bottom) ->
        do eH <- interp (If (Bol vH) thn els) e
           return (FacetV p eH Bottom)

      (FacetV p Bottom (Boolean vL)) ->
        do eL <- interp (If (Bol vL) thn els) e
           return (FacetV p Bottom eL)

      (FacetV p Bottom Bottom) ->
          return (FacetV p Bottom Bottom)

      otherwise -> proceed args


-- helpers

------------------------------ DEREF
deref :: Value -> M Value
deref value = goDeref # value -- i13n

-- i13n
goDeref :: Value -> M Value
goDeref t =
    case t of
      Bottom      -> return Bottom
      (Address a) -> do store <- lift $ lift $ get
                        return (storeLookup a store)
      otherwise   -> return (Error "deref: not an address or bottom")

-- i13n
goDerefAdv proceed t =
    case t of
      (FacetV p vH vL) -> do progCounter <- get
                             goDerefAdvHelper t progCounter
      otherwise -> proceed t

goDerefAdvHelper :: Value -> ProgCounter -> M Value
goDerefAdvHelper (FacetV p vH vL) [] =
    do vh <- deref vH
       vl <- deref vL
       return (createFacetValue [p] vh vl)

goDerefAdvHelper f@(FacetV p@(_,neg) vH vL) (h:rest) =
    if h == p
       then if neg == True
            then deref vH
            else deref vL
    else goDerefAdvHelper f rest


------------------------------ ASSIGN
assign :: Value -> Value -> M Value
assign left right = goAssign # (left, right) -- i13n

-- i13n
goAssign :: (Value, Value) -> M Value
goAssign (left, right) =
    case left of
      Bottom -> return Bottom

      (Address a) -> do store <- lift $ lift $ get
                        (lift . lift . put) (storeReplace a right store)
                        return right

      otherwise -> return (Error "assign: not an address or bottom")

-- i13n
goAssignAdv proceed args@(left, right) =
    case left of
      (FacetV (p,neg) vH vL) -> do progCounter <- get
                                   put ((p,neg):progCounter)
                                   r1 <- assign vH right
                                   put ((p, not neg):progCounter)
                                   r2 <- assign vL right
                                   put progCounter
                                   return right

      (Address a) -> do store <- lift $ lift $ get
                        progCounter <- get
                        let v = storeLookup a store
                        let fv = createFacetValue progCounter right v
                        (lift . lift . put) (storeReplace a fv store)
                        return right

      otherwise -> proceed args


envLookup :: Name -> Environment -> Value
envLookup x [] = (Error ("unbound " ++ show x))
envLookup x ((y,b):e) = if x == y then b else envLookup x e

storeLookup :: Address -> Store -> Value
storeLookup a [] = (Error ("not in store " ++ show a))
storeLookup a ((b,v):s) = if a == b then v else storeLookup a s

storeReplace :: Address -> Value -> Store -> Store
storeReplace a v [] = []
storeReplace a v ((b,w):s) = if a == b then ((a,v):s)
                             else ((b,w):(storeReplace a v s))

apply :: Value -> Value -> M Value
apply (Closure f) a = f a
apply Bottom _      = return Bottom
apply _ _           = return (Error "apply: not a closure or bottom")

-- testing

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
termBot = (App Bot (Con 1))
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

facetTest3 = (Let "x" (Ref (Bol True))
              (Assign (Var "x") (Bol False)))

fentonTestRaw = (Let "x" (Ref (Bol True))
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

fentonTest = (Let "x" (Ref (Facet (1,True) (Bol True) Bot))
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

-- let x = ref (<1 ? true : ⟂>) in (
--   let y = ref true in (
--     let z = ref true in (
--       if !x then y := false;  # y = <1 ? false : true>
--       if !y then z := false;  # z = <1 ? true : false>
--         !z)))
