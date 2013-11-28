{-# LANGUAGE TemplateHaskell,
             DeriveDataTypeable,
             ScopedTypeVariables             
 #-}

module AOP.Internal.Function
(
 Function(..),
 FunctionTag,
 mkFunction,
 defaultFunctionTag,
--  newTag
) where

import AOP.Internal.Typeable1Monad
-- -- import AOP.Internal.THMacros

-- {- | We define a notion of function equality based on tagging functions with identifiers: two functions are 
-- equal if they have the same identifier. We extend join point definition and weaving to support these tags. To 
-- provide compatibility with regular functions (which use StableNames for equality) we use a special default 
-- tag in join points and weaving, to distinguish regular functions from tagged functions and use the 
-- corresponding notion of equality.
-- -}

type FunctionTag = Integer

data Function a b = Function {fun :: a -> b, tag :: FunctionTag} deriving Typeable

-- -- | Default tag to be used for regular functions. Our overloaded definitions detect this tag and
-- -- use StableNames for function equality. newTag is a TemplateHaskell macro that is replaced by a unique integer (compilation-wide) at compile time.
defaultFunctionTag =  439043 -- $newTag

-- | Wraps a regular function into a wrapped one.
mkFunction :: (a -> b) -> FunctionTag -> Function a b
mkFunction f t = Function f t

-- | Equality of wrapped function is based on tag comparison. By construction tags are already instances of Eq.
instance Eq (Function a b) where
         Function _ tag == Function _ tag' = tag == tag'