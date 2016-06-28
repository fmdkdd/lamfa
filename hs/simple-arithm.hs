{-# LANGUAGE ExistentialQuantification #-}

data Term = Constant Int
          | Plus Term Term

eval :: Term -> Int
eval (Constant n) = n
eval (Plus e1 e2) = eval e1 + eval e2

-- New variant

data Term2 = Constant2 Int
           | Plus2 Term2 Term2
           | Pair2 Int Int

data R = forall a. Show a => R a
instance Show R where show (R a) = show a

eval2 :: Term2 -> R
eval2 (Constant2 n) = R n
--eval2 (Plus2 e1 e2) = eval2 e1 + eval2 e2
eval2 (Pair2 a b) = R (a,b)
