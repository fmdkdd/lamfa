type Name = String

opendata Term = Bot
               | Con Int
               | Lam Name Term
  deriving Show

opendata Value = Bottom
                | Constant Int
                | Closure Name Term
  deriving Show

eval :: Term -> Value
eval Bot           = Bottom
eval (Con i)       = Constant i
eval (Lam x v)     = Closure x v

-- Tests

term0 = (Lam "x" Bot)


-- Extension

extenddata Term where
    P Term Term

extenddata Value where
   Pair Value Value

extend eval where
  eval (P t1 t2) = Pair (eval t1) (eval t2)

-- tests
term1 = P (Con 1) (Con 2)
