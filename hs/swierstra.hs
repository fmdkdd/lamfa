{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeOperators         #-}

-- Re-reading Swierstra in 2015.
-- "Data types à la carte", Wouter Swierstra (2008).

-- 'f' is the signature of the constructors we wish to fix; 'f' has a type
-- parameter: the subtrees to the contsructor.
data Expr f = In (f (Expr f))

data Val e = Val Int deriving (Functor, Show)
data Add e = Add e e deriving (Functor, Show)

data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Functor, Show)

test1 :: Add (Val e)
test1 = Add (Val 2) (Val 2)

-- Can type, but all programs have different types.
test2 :: Add (Either (Val e) (Add (Val e1)))
test2 = Add (Left (Val 3)) (Right (Add (Val 0) (Val 0)))



val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add x y = inject (Add x y)

data Mul e = Mul e e deriving (Functor, Show)
mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f
mul x y = inject (Mul x y)

data Zero e = Zero deriving (Functor, Show)
zero :: (Zero :<: f) => Expr f
zero = inject Zero

-- Can we inject f in g?
class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a
-- We can inject f into f trivially
instance Functor f => f :<: f where
  inj = id
-- We can inject f into the coproduct of f and g, by wrapping it with Inl.
-- Think of inserting f in the head of the list.
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
-- We can inject f into the coproduct of h and g, if we can inject f in g, by
-- putting f on the right after injecting in g.  Think of inserting into the
-- tail of a list (recursive case).
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

-- Here is how to automatically build expression with the right constructors.
inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj


-- The :+: data type has the shape of a tree, but we resolve the injection by
-- traversing it as a list.  This means that we can type this:
types1 :: Expr (Add :+: (Val :+: Mul))
types1 = add (val 30000) (mul (val 1) (val 1330))

-- but not this:
fail1 :: Expr ((Add :+: Val) :+: Mul)
fail1 = add (val 30000) (mul (val 1) (val 1330))
-- Although the infix operator would lead us to believe so.  Why is this
-- important?  Because we might want to bundle related terms together as one
-- type, e.g.:
type Arith = Add :+: Val

-- And this would fail to type, while reversing the arguments to :+: would
-- type.  Don't use :+: when the operands do not commute!
fail2 :: Expr (Arith :+: Mul)
fail2 = add (val 30000) (mul (val 1) (val 1330))

-- But we can actually add the required instances, by forcing the compiler to
-- look into the left-hand side of :+:.  And now it types.
instance (Functor f, Functor g, Functor h) => f :<: ((f :+: h) :+: g) where
  inj = Inl . inj
instance (Functor f, Functor g, Functor h) => f :<: ((h :+: f) :+: g) where
  inj = Inl . inj

-- We can compose two sub-languages.
fail3 :: Expr ((Add :+: Val) :+: (Mul :+: Zero))
fail3 = add zero (mul (val 1) (val 1330))

-- Of course we can continue to put out idiotic instances of the sort, using a
-- macro or template Haskell.

-- instance (Functor f, Functor g, Functor h) => f :<: ((g :+: h) :+: f) where
--   inj = Inr . inj
-- instance (Functor f, Functor g, Functor h1, Functor h2, f :<: h2) => f :<: ((h1 :+: h2) :+: g) where
--   inj = Inl . Inr . inj
-- instance (Functor f, Functor g, Functor h1, Functor h2) => f :<: ((h1 :+: h2) :+: (g :+: f)) where
--   inj = Inr . Inr . inj
-- instance (Functor f, Functor g, Functor h1, Functor h2) => f :<: ((h1 :+: h2) :+: (f :+: g)) where
--   inj = Inr . Inl . inj

-- Another approach would be to define terms not independently, but by
-- languages, as to avoid using type aliases for related terms.

data Arith2 e = Const Int | Plus e e deriving (Functor, Show)
c :: (Arith2 :<: f) => Int -> Expr f
c x = inject (Const x)

plus :: (Arith2 :<: f) => Expr f -> Expr f -> Expr f
plus x y = inject (Plus x y)

instance Eval Arith2 where
  evalAlgebra (Const x) = x
  evalAlgebra (Plus x y) = x + y

test5 :: Expr (Arith2 :+: Mul)
test5 = plus (c 1) (mul (c 2) (c 3))


class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

instance Eval Zero where
  evalAlgebra Zero = 0

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- Another algebra will give a slightly different interpretation We can define
-- it to be like evalAlgebra, by default.
class Eval f => Eval2 f where
  eval2Algebra :: f Int -> Int
  eval2Algebra = evalAlgebra

-- Then change its meaning for only one term.
instance Eval2 Val where
  eval2Algebra v = 1 + evalAlgebra v

-- This is required for eval2Algebra to be called recursively on the coproduct.
instance (Eval2 f, Eval2 g) => Eval2 (f :+: g) where
  eval2Algebra (Inl x) = eval2Algebra x
  eval2Algebra (Inr y) = eval2Algebra y

-- Contrast with simply writing
-- instance (Eval2 f, Eval2 g) => Eval2 (f :+: g) where {}
-- Here we would get `eval2 test6` == 1 but `eval2 test6 == 0`.

-- Haskell requires that theses instances be explicitly declared.  Even though
-- the definition of the class Eval2 implies that we can call Eval2 on anything
-- we can call evalAlgebra on.
instance Eval2 Add
instance Eval2 Mul

-- We can dispense of the two previous lines by providing a "catch-all"
-- instance.  Unfortunately, it is undecidable, and needs the
-- UndecidableInstances flag, which is kind of unsafe.
-- instance (Eval f) => Eval2 f where
--   eval2Algebra = evalAlgebra

eval2 :: Eval2 f => Expr f -> Int
eval2 = foldExpr eval2Algebra

test6 :: Expr Val
test6 = val 0
-- eval  test6 == 0
-- eval2 test6 == 1

test7 :: Expr (Add :+: Val)
test7 = add (val 0) (val 0)
-- eval  test7 == 0
-- eval2 test7 == 2

test8 :: Expr (Add :+: Val)
test8 = add (add (val 0) (val 0)) (add (val 0) (val 0))
-- eval  test8 == 0
-- eval2 test8 == 4

test9 :: Expr (Mul :+: Val :+: Add)
test9 = mul (val 1) (add (val 0) (val 0))
-- eval  test9 == 0
-- eval2 test9 == 4
