-- Combine list monad and state monad for faceted values?

type Name      = String
type Address   = Int

data Term = Bot
          | Con Int
          | Bol Bool
          | Var Name
          | Lam Name Term
          | App Term Term
          | Ref Term
          deriving Show

data Value = Bottom
           | Constant Int
           | Boolean Bool
           | Address Int
           | Closure Name Term Environment
           deriving Show

type Environment = [(Name, Value)]
type Store = [(Address, Value)]


type Principal = Int
data Facet = Facet Principal Facet Facet | Raw Value

-- data State s v = State { run :: s -> (s,v) }

-- instance Monad (State s) where
--   return a = State $ \s -> (s,a)
--   (State f) >>= g = State $ \s0 -> let (s1,v) = f s0
--                                    in run (g v) s1

-- get :: State s s
-- get = State $ \s -> (s,s)


-- instance Monad Facet where
--   return = Raw
--   (Raw a) >>= g = Raw (g a)
--   (Facet k h l) >>= g = get >>= \pc -> return $ apply pc
--     where apply pc | k `elem` pc = Facet k (g h) l
--                    | otherwise   = Facet k h (g l)


type ProgramCounter = [Principal]
-- type PC a = State ProgramCounter a
-- type FV = Facet Value

-- One monad for both, as the bind for faceted values needs to access
-- the PC?
type F = ProgramCounter -> (Facet, ProgramCounter)

pure :: a -> (ProgramCounter -> (a, ProgramCounter))
pure a = \pc -> (a, pc)

bind :: F -> (Value -> Facet) -> F
bind f g = \pc0 -> let (facet, pc1) = f pc0
                   in case facet of
                     Raw a -> (g a, pc1)
                     _     -> (apply g facet, pc1)
                       where apply g' (Facet k h l) =
                                if k `elem` pc1
                                then Facet k (apply g' h) l
                                else Facet k h (apply g' l)


-- Can `apply` ignore the fact that values are faceted, if we call it
-- inside the faceted evaluation monad?  Have `apply` be a function
-- inside the monad?  Ad-hoc polymorphism?
eval :: Term -> Environment -> Store -> M Value

eval (App t u) e =
  do f <- eval t e
     a <- eval u e
     apply f a

apply :: Value -> Value -> M Value
apply Bottom _               = return Bottom
apply (Closure x body env) v = eval body ((x,v):env)

instance Evaluable v where
  apply f = undefined
  deref f = undefined

-- eval :: Term -> Environment -> F
-- eval Bot _ = Raw Bottom




main = undefined
