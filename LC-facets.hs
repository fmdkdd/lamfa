import Control.Monad.State
import Control.Monad.Identity

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
          | Facet Principal Term Term
   deriving Show

data Value = Error String
           | Bottom
           | Constant Int
           | Boolean Bool
           | Address Int
           | Closure Name Term Environment
           | FacetV Principal Value Value

type Principal = Int
type Branch    = (Principal,Bool)

createFacetValue :: ProgCounter -> Value -> Value -> Value
createFacetValue [] vH vL                 = vH
createFacetValue ((k,True):rest)  vH vL = FacetV k (createFacetValue rest vH vL) vL
createFacetValue ((k,False):rest) vH vL = FacetV k vL (createFacetValue rest vH vL)

instance Eq Value where
  Error s1 == Error s2               = s1 == s2
  Bottom == Bottom                   = True
  Constant i1 == Constant i2         = i1 == i2
  Boolean b1 == Boolean b2           = b1 == b2
  Address i1 == Address i2           = i1 == i2
  Closure _ _ _ == Closure _ _ _     = False
  FacetV p v1 v2 == FacetV q w1 w2   = p == q && v1 == w1 && v2 == w2

instance Show Value where
  show (Error s)            = "<Error> " ++ show s
  show Bottom               = "<bottom>"
  show (Constant i)         = show i
  show (Boolean b)          = show b
  show (Address i)          = "<address> " ++ show i
  show (Closure "x" (Lam "y" (Var "x")) _) = show True
  show (Closure "x" (Lam "y" (Var "y")) _) = show False
  show (Closure _ _ _)      = "<closure>"
  show (FacetV p v1 v2)     = "{" ++ show p ++ ", " ++ show v1 ++ ", " ++ show v2 ++ "}"

type Environment = [(Name, Value)]
type Store = [(Address, Value)]
type ProgCounter = [Branch] -- i13n

type M a = (StateT ProgCounter (StateT Store Identity)) a

runM :: M Value -> ProgCounter -> Store -> ((Value, ProgCounter), Store)
runM m pc s = runIdentity (runStateT (runStateT m pc) s)

interp :: Term -> Environment -> M Value
interp Bot e         = return Bottom
interp (Con i) e     = return (Constant i)
interp (Var x) e     = return (envLookup x e)
interp (Lam x v) e   = return (Closure x v e)

-- should yield terms rather than values, see rule F-IF-SPLIT
interp (Facet p t1 t2) e =
  do vH <- interp t1 e
     vL <- interp t2 e
     return (FacetV p vH vL)

interp (App t u) e =
  do f <- interp t e
     a <- interp u e
     apply f a

interp (Ref t) e =
    do v' <- interp t e
       store <- (lift $ get)
       let addr = length store
       progCounter <- get
       let v = createFacetValue progCounter v' Bottom
       (lift. put) ((addr,v):store)
       return (Address addr)

interp (Deref t) e =
  do v <- interp t e
     deref v

interp (Assign l r) e =
  do lv <- interp l e
     rv <- interp r e
     assign lv rv

-- desugaring
interp (Let id namedExpr body) e = interp (App (Lam id body) namedExpr) e
interp (Seq left right) e        = interp (Let "freevar" left right) e
interp (If cond thn els) e       = interp (App
                                           (App
                                            (App cond (Lam "d" thn))
                                            (Lam "d" els))
                                           (Lam "x" (Var "x"))) e
interp (Bol True) e              = interp (Lam "x" (Lam "y" (Var "x"))) e
interp (Bol False) e             = interp (Lam "x" (Lam "y" (Var "y"))) e


-- helpers

deref :: Value -> M Value
deref Bottom           = return Bottom
deref (Address a)      = do store <- lift $ get
                            return (storeLookup a store)
deref (FacetV p vH vL) = do progCounter <- get
                            if (p,True) `elem` progCounter
                            then deref vH
                            else if (p,False) `elem` progCounter
                                 then deref vL
                                 else do vH' <- deref vH
                                         vL' <- deref vL
                                         return (createFacetValue [(p,True)] vH' vL')

assign :: Value -> Value -> M Value
assign Bottom _              = return Bottom
assign (Address a) right     = do store <- lift $ get
                                  progCounter <- get
                                  let v = storeLookup a store
                                  let fv = createFacetValue progCounter right v
                                  (lift . put) (storeReplace a fv store)
                                  return right

assign (FacetV p vH vL) right = do progCounter <- get
                                   put ((p,True):progCounter)
                                   r1 <- assign vH right
                                   put ((p,False):progCounter)
                                   r2 <- assign vL right
                                   put progCounter
                                   return right

apply :: Value -> Value -> M Value
apply Bottom _               = return Bottom
apply (Closure x body env) v = interp body ((x,v):env)
apply (FacetV p vH vL) v2    = do progCounter <- get
                                  if (p,True) `elem` progCounter
                                  then apply vH v2
                                  else
                                      if (p,False) `elem` progCounter
                                      then apply vL v2
                                      else do put ((p,True):progCounter)
                                              vH' <- apply vH v2
                                              put ((p,False):progCounter)
                                              vL' <- apply vL v2
                                              put progCounter
                                              return (createFacetValue [(p,True)] vH' vL')

-- other helpers
envLookup :: Name -> Environment -> Value
envLookup x env = case (lookup x env) of
                    Just v -> v
                    Nothing -> (Error ("unbound " ++ show x))

storeLookup :: Address -> Store -> Value
storeLookup a store = case (lookup a store) of
                        Just v -> v
                        Nothing -> (Error ("not in store " ++ show a))

storeReplace :: Address -> Value -> Store -> Store
storeReplace a v [] = []
storeReplace a v ((b,w):s) = if a == b then ((a,v):s)
                             else ((b,w):(storeReplace a v s))

-- testing

-- use implicit parameters??

test :: Term -> Environment -> ProgCounter -> Store -> String
test t env pc store = show (runM (interp t env) pc store)

testDefault t = test t [] [] []

testWithPC :: Term -> ProgCounter -> String
testWithPC t pc = show (runM (interp t []) pc [])

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
facetTest1 = (If (Facet 1 (Bol True) (Bol False)) (Con 42) (Con 24))

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

-- let x = ref (<1 ? true : ⟂>) in (
--   let y = ref true in (
--     let z = ref true in (
--       if !x then y := false;  # y = <1 ? false : true>
--       if !y then z := false;  # z = <1 ? true : false>
--         !z)))
