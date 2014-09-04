{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

data Term f a =
    Pure a
  | Impure (f (Term f a))

instance Functor g => Functor (Term g) where
  fmap f (Pure x)    = Pure (f x)
  fmap f (Impure t)  = Impure (fmap (fmap f) t)

-- Particular functors give raise to particular monads:
instance Functor g => Monad (Term g) where
  return x        = Pure x
  (Pure x)   >>= f  = f x
  (Impure t) >>= f  = Impure (fmap (>>= f) t)

data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance (Functor f) => f :<: f where
 inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
 inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
 inj = Inr . inj


data Incr t = Incr Int t deriving Functor

data Recall t = Recall (Int -> t) deriving Functor

inject :: (g :<: f) => g (Term f a) -> Term f a
inject = Impure . inj
  
incr :: (Incr :<: f) => Int -> Term f ()
incr i = inject (Incr i (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = inject (Recall Pure)

-- explicitly works in Term (Recall :+: Incr)
tick :: Term (Recall :+: Incr) Int
tick = do y <- recall
          incr 1
          return y

-- generic for any Term f monad that 'implements' Recall and Incr
tick' :: (Recall :<: f, Incr :<: f) => Term f Int
tick' = do y <- recall
           incr 1
           return y

-- Term  = Pure a | Impure (f (Term f a))

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure _ (Pure x)  = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

{-
t ::  (f (Term f a))
foldTerm pure imp :: Term f a -> b
fmap :: (Term f a) -> b -> f (Term f a) -> f b
-}

-- "We must still define a suitable algebra to pass to the foldTerm function"

-- What is an Algebra???? :(

newtype Mem = Mem Int deriving Show

run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra


-- looks specific to the semantics/effects
-- maybe here we can use regular monads/transformers

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run Incr where
  runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
  runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

example1 = run tick (Mem 4)
example2 = run (tick' :: Term (Recall :+: Incr) Int) (Mem 4)

------------------------------------------------------------------------------------
-- Applications



data Teletype a =
    GetChar (Char -> a)
  | PutChar Char a
 deriving Functor

data FileSystem a =
    ReadFile FilePath (String -> a)
  | WriteFile FilePath String a
 deriving Functor

exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

class Functor f => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra (GetChar f)    = Prelude.getChar >>= f
  execAlgebra (PutChar c io) = Prelude.putChar c >> io

instance Exec FileSystem where
  execAlgebra (ReadFile fp f)     = Prelude.readFile fp >>= f
  execAlgebra (WriteFile fp s f)  = Prelude.writeFile fp s >> f

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra (Inl r) = execAlgebra r
  execAlgebra (Inr r) = execAlgebra r  

-- instance Exec FileSystem where
--   exec
getChar :: (Teletype :<: f) => Term f Char
getChar = inject (GetChar Pure)

putChar :: (Teletype :<: f) => Char -> Term f ()
putChar c = inject (PutChar c (Pure ()))

-- readFile :: (Filesystem :<: f) => 
  

-- cat :: Term (Teletype :+: FileSystem) ()
cat :: Term (Teletype) ()
cat = do
  -- contents <- readFile fp
  -- mapM putChar contents
  c <- Main.getChar
  return ()
