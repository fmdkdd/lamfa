data Term = Plus Term Term | Constant Int | Couple Int Int
data Value = Single Int | Pair Int Int
  deriving Show

interp :: Term -> Value
interp (Plus t1 t2) = plus (interp t1) (interp t2)
interp (Constant i) = (Single i)
interp (Couple i1 i2) = (Pair i1 i2)

plus :: Value -> Value -> Value
plus (Single a) (Single b) = (Single (a + b))
plus (Single a) (Pair i1 i2) = (Pair (a + i1) (a + i2))
