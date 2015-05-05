module Base where

opendata Term = Con Int

opendata Value = Constant Int

eval :: Term -> Value
eval (Con i) = Constant i
