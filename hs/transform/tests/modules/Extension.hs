module Extension where

extenddata Term where
  P Term Term

extenddata Value where
  Pair Value Value

extend eval where
  eval (P t1 t2) = Pair (eval t1) (eval t2)
