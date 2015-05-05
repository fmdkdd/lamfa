{-# LANGUAGE ImplicitParams #-}

type Name = String
type Address = Int

data Term = Bot
          | C Int
          | V Name
          | Fun Name Term
          | App Term Term
          | Ref Term
          | Deref Term
          deriving (Show, Eq)

data Value = Bottom
           | Constant Int
           | Address Address
           | Closure Name Term Environment
           | Error String
           | Facet Principal Value Value
           deriving (Show, Eq)

type Environment = [(Name, Value)]
type Store = [(Address, Value)]

eval :: Store -> Environment -> Term -> (Store, Value)
eval s env (C e)      = (s, Constant e)

eval s env (V x)      = (s, v)
  where v = case lookup x env of
          Just v -> v
          Nothing -> Error ("unbound " ++ show x)

eval s env (Fun x e)  = (s, Closure x e env)

eval s env (App e1 e2) = apply s2 v1 v2
  where (s1, v1) = eval s env e1
        (s2, v2) = eval s1 env e2

eval s env (Ref e) = (s2, Address a)
  where s2 = (a, v1):s1
        a = length s1
        (s1, v1) = eval s env e

eval s env (Deref e) = (s1, deref s1 v1)
  where (s1, v1) = eval s env e

apply :: Store -> Value -> Value -> (Store, Value)
apply s Bottom _            = (s, Bottom)
apply s (Closure x e env) v = eval s ((x,v):env) e

deref :: Store -> Value -> Value
deref _ Bottom      = Bottom
deref s (Address a) = case lookup a s of
  Just v -> v
  Nothing -> Error ("not in store" ++ show a)

stdEval :: Store -> Environment -> Term -> (Store, Value)
stdEval = eval

-- Tests

test1 = stdEval [] [] (App (Fun "x" (Deref (V "x"))) (Ref (C 42)))
        == ([(0, Constant 42)], Constant 42)


-- Facets

type Principal = Int
type PC = [Principal]

-- extend Value with Facet

mkFacet :: PC -> Value -> Value -> Value
mkFacet [] v1 _ = v1
mkFacet (k:ks) v1 v2 | k > 0 = Facet k (mkFacet ks v1 v2) v2
                     | otherwise = Facet k v2 (mkFacet ks v1 v2)

-- override original eval definition
-- with aspects?

facetsEval :: (?pc :: PC) =>
              Store -> Environment -> Term -> (Store, Value)
facetsEval s env (Ref e) = (s2, Address a)
  where s2 = (a, v2):s1
        a = length s1
        v2 = mkFacet ?pc v1 Bottom
        (s1, v1) = facetsEval s env e
facetsEval s env t = stdEval s env t

test2 = let ?pc = [1] in
  facetsEval [] [] (App (Fun "x" (Deref (V "x"))) (Ref (C 42)))

-- Gives control to stdEval too early.  Maybe use typeclasses instead of
-- recursive functions to simulate OO-dispatching on term type.
