{-# LANGUAGE MultiParamTypeClasses #-}

-- Cannot be more general than that ...
class Eval t v where
    eval :: t -> v

data Term = Bot
  deriving Show

data Value = Bottom
  deriving Show

instance Eval Term Value where
    eval Bot = Bottom

-- Have to specify second param, otherwise `eval` call is ambiguous
test1 :: Value
test1 = eval Bot


-------------
-- More sophisticated example

data FValue = Raw Value | Complex FValue Int
  deriving Show

instance Eval Term FValue where
    eval v = Raw $ eval v
--    eval (Complex fv i) = Complex (eval fv) i

test2 :: FValue
test2 = eval Bot
