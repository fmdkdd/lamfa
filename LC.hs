import Control.Monad.State
import Control.Monad.Identity


type Name = String
type Address = Int
 
data Term = Bot
          | Con Int
          | Var Name
          | Lam Name Term
          | App Term Term
          | Ref Term
          | Deref Term
          | Assign Term Term
 
data Value = Error String
           | Bottom
           | Constant Int
           | Address Int
           | Closure (Value -> M Value)

instance Show Value where
  show (Error s)    = "<Error> " ++ show s
  show Bottom       = "<bottom>"
  show (Constant i) = show i
  show (Address i)  = "<address> " ++ show i
  show (Closure f)  = "<function>"

  
type Environment = [(Name, Value)]
type Store = [(Address, Value)]

type M a = (StateT Store Identity) a

run :: M Value -> Store -> Value
run m s = runIdentity (evalStateT m s)

interp :: Term -> Environment -> M Value
interp Bot e       = return Bottom
interp (Con i) e   = return (Constant i)
interp (Var x) e   = return (envLookup x e)
interp (Lam x v) e = return (Closure (\a -> interp v ((x,a):e)))
          
interp (App t u) e = 
  do f <- interp t e
     a <- interp u e
     apply f a
                          
interp (Ref t) e =
  do v <- interp t e
     store <- get
     let addr = length store
     put ((addr,v):store)
     return (Address addr)

interp (Deref t) e =
  do v <- interp t e
     deref v

interp (Assign l r) e =
  do lv <- interp l e
     rv <- interp r e
     assign lv rv
          


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
deref (Address a) = do store <- get
                       return (storeLookup a store)
deref _           = return (Error "deref: not an address or bottom")
 
assign :: Value -> Value -> M Value
assign Bottom _      = return Bottom
assign (Address a) v = do store <- get
                          put (storeReplace a v store)
                          return v

assign _ _ = return (Error "assign: not an address or bottom")
 
test :: Term -> String
test t = show (run (interp t []) [])

-- Tests, we skip them for now!

term0 = (App (Lam "x" (Var "x")) (Con 10))
termBot = (App (Bot) (Con 1))
termStore = (Deref (App (Lam "x" (App (Lam "y" (Var "x"))
                                      (Assign (Var "x") (Con 2))))
                        (Ref (Con 1))))