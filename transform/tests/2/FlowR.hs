{-# LANGUAGE FlexibleContexts #-}
module FlowR (
  Privilege(..),
  Tag,
  Label,
  Labels(..),
  TraceData(..),
  Trace(..),
  TraceR,
  allowCheck,
  addDefault,
  propagate,
  printTrace,
  ) where

import Control.Monad.Writer

import Data.List (intercalate, deleteFirstsBy, unionBy)

data Privilege = Plus | Minus
               deriving Eq

instance Show Privilege where
    show Plus = "+"
    show Minus = "-"

type Tag = (String, Privilege)
type Label = [Tag]
data Labels = Labels Label Label
            | Wildcard
            deriving Eq

instance Show Labels where
    show (Labels r s) = "(" ++ showLabel r ++ "," ++ showLabel s ++ ")"
    show Wildcard = "*"

showLabel :: Label -> String
showLabel ts = "[" ++ (intercalate " " $ map showTag ts) ++ "]"

showTag :: Tag -> String
showTag ("default",p) = "δ" ++ show p
showTag (n,p) = n ++ show p

showSend :: Labels -> String
showSend Wildcard = "*"
showSend (Labels _ s) = showLabel s

showReceive :: Labels -> String
showReceive Wildcard = "*"
showReceive (Labels r _) = showLabel r

data TraceData a = Allow Labels Labels Bool
                 | Propagate Labels Labels Labels
                 | Return a
                 | Call Labels Labels Labels
                 | Str String

instance Show a => Show (TraceData a) where
    show (Allow l1 l2 b) =
        "Allow " ++ showSend l1 ++ " " ++ showReceive l2 ++ " -> " ++ show b
    show (Propagate l1 l2 l3) =
        "Propagate " ++ showSend l1 ++ " " ++ showSend l2 ++ " -> " ++ showSend l3
    show (Return v) = "R := " ++ show v
    show (Call c f v) = "Apply f:" ++ show f
                        ++ " to v:" ++ show v
                        ++ " caller:" ++ show c
    show (Str s) = s

data Trace a = Line Int a
             | BeginBlock
             | EndBlock
  deriving Show

type TraceR a = [Trace (TraceData a)]

allowCheck :: (MonadWriter (TraceR a) m) => Int -> Labels -> Labels -> m ()
allowCheck step l1 l2 = tell [Line step $ Allow (addDefault l1) (addDefault l2) allowed] >>
                        when (not allowed) (fail "invalid flow")
    where allowed = allow l1 l2

allow :: Labels -> Labels -> Bool
allow Wildcard _ = True
allow _ Wildcard = True
allow (Labels _ s) (Labels r _) = all present s'
    where present (n, _) = case (lookup n r') of
                             Just Plus -> True
                             _ -> False
          s' = addDefaultS s
          r' = addDefaultR r

addDefault :: Labels -> Labels
addDefault (Labels r s) = Labels (addDefaultR r) (addDefaultS s)
addDefault Wildcard = Wildcard

addDefaultS :: [Tag] -> [Tag]
addDefaultS [] = [("default",Plus)]
addDefaultS s = s

addDefaultR :: [Tag] -> [Tag]
addDefaultR r = case (lookup "default" r) of
                  Just _ -> r
                  Nothing -> ("default",Plus):r

propagate :: Labels -> Labels -> Labels
propagate Wildcard b = b
propagate _ Wildcard = Wildcard
propagate (Labels _ sa) (Labels rb sb) = Labels rb sb'
    where sb' = let p = filter plusTag sa
                    n = filter minusTag sa
                    r = deleteFirstsBy tagEq sb n
                in unionBy tagEq r p -- order of `r` and `p` matters
                                     -- equality is not symmetric

tagEq :: Tag -> Tag -> Bool
tagEq (n, _) (m, _) = n == m

plusTag :: Tag -> Bool
plusTag (_,Plus) = True
plusTag _ = False

minusTag :: Tag -> Bool
minusTag (_,Minus) = True
minusTag _ = False

printTrace :: Show a => (TraceR a) -> Int -> IO ()
printTrace [] _ = return ()
printTrace ((Line i s):ts) n = do putStrLn $ (nspaces n) ++ show i ++ ". " ++ show s
                                  printTrace ts n
printTrace ((BeginBlock):ts) n = do printTrace ts (n + 2)
printTrace ((EndBlock):ts) n = do printTrace ts (n - 2)

nspaces :: Int -> String
nspaces n = concat $ replicate n " "


-- liftR :: (Monad m) => m Value -> m Value
-- liftR mx = mx >>= \x ->
--            case x of
--              (ValueR _ _) -> return x
--              v -> return (ValueR x (Labels [] []))
