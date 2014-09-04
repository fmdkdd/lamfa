{-# LANGUAGE ExistentialQuantification #-}

class Eval e where
    eval :: e -> Int

data BaseTerm e = Bot
                | Rec e

instance Eval e => Eval (BaseTerm e) where
    eval Bot = 1
    eval (Rec t) = eval t

data Term = forall e. Eval e => Term e

instance Eval Term where
    eval = \(Term t) -> eval t

data Sig = Sig {
      f :: Term -> Int
      creer :: e ->
    }

test :: Sig -> Int
test (Sig f) = f $ creer Bot
