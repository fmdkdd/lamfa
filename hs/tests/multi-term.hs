{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

data Term = Con Int
          | Plus Term Term

type Value = Int

class Exec f where
    exec :: f -> Value

instance Exec Term where
    exec (Con x) = x
    exec (Plus a b) = (exec a) + (exec b)

ex = (Plus (Con 3) (Plus (Con 2) (Con 3)))
res1 = exec ex
-- expects 8


newtype TermM = TermM { aTerm :: Term }

instance Exec TermM where
    exec (TermM (Con x)) = x
    exec (TermM (Plus a b)) = (exec a) * (exec b)

exM = TermM ex
res2 = exec exM
-- expects 18, got 15
-- Fails because the variant exec is not called recursively


class ExecM f where
    execM :: f -> Value

instance ExecM Term where
    execM (Con x) = x
    execM (Plus a b) = (execM a) * (execM b)

res3 = execM ex
-- expects 18, got 18


{- Data types à la carte + different execution algebra = terms
   extension and semantics redefinition
-}

class Eval f v where
    eval :: f -> v

instance Eval Term Int where
    eval (Con x) = x
    eval (Plus a b) = (+) (eval a) (eval b)

instance Eval Term String where
    eval (Con x) = show x
    eval (Plus a b) = (++) (eval a) (eval b)

-- eval ex :: Int
-- gives 8
-- eval ex :: String
-- gives "323"

-- Multi-parameters type classes are somewhat inconvenient.  The same
-- can be achieved with a typeclass for each value, with different
-- function names.  No need to disambiguate the call to eval then.

main :: IO ()
main = undefined
