--data Bottom = Bot

data Bottom = Bot
data RecTerm e = Rec e
-- data BaseTerm = ()

data ExtendedTerm e f = Pair e f

-- data Term e where
--     Base :: e -> Term (BaseTerm e)
--     Extended :: e -> Term (ExtendedTerm e)

class Eval f where
    eval :: f -> Int

-- instance Eval e => Eval (Term e) where
--     eval (Base e) = eval e

instance Eval Bottom where
    eval Bot = 1

-- instance Eval () where
--     eval () = 0

instance Eval e => Eval (RecTerm e) where
--    eval Bot = 1
    eval (Rec t) = eval t

instance (Eval e, Eval f) => Eval (ExtendedTerm e f) where
    eval (Pair t1 t2) = (eval t1) + (eval t2)

-- newtype Expr e = Expr (e ())

-- instance Eval e => Eval (Expr e) where
--     eval (Expr t) = eval t

-- instance Eval B where
--     eval t = eval t

-- eval :: Eval e -> e -> Int
-- eval (Pair t1 t2) = (eval t1) + (eval t2)
-- eval (Rec t) = eval t
-- eval (Base Bot) = 1

--term0 :: RecTerm ()
term0 = Bot

--term00 :: BaseTerm (BaseTerm ())
term00 = Rec Bot

--term1 :: ExtendedTerm (BaseTerm ())
term1 = Pair Bot Bot

--term2 :: ExtendedTerm (BaseTerm (ExtendedTerm (BaseTerm ())))
term2 = Pair (Rec (Pair Bot Bot)) Bot
-- term1 = (Facet (Toto (Rec (Facet (Toto (Rec (Toto Bot)))))))

--
-- data Nat = Zero | Succ Nat

-- nat0 = Zero
-- nat1 = Succ Zero
