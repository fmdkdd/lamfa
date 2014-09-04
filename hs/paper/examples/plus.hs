data Term = Plus Term Term | Constant Int
type Value = Int

interp :: Term -> Value
interp (Plus t1 t2) = plus (interp t1) (interp t2)
interp (Constant i) = i

plus :: Value -> Value -> Value
plus = (+)
