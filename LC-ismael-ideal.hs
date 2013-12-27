{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.State
import Control.Monad.Identity
import AOP.Default
import Data.Typeable
import LCS hiding (test, testDefault)

import Unsafe.Coerce

-- data extensions: using Either as in Monad Transformers and Modular Interpreters

-- new terms
data FacetTerm  = Facet Principal Term Term
newtype NewTerm = NewTerm (Either Term FacetTerm)

-- new values
data FacetValue = FacetV Principal NewValue NewValue
newtype NewValue = NewValue (Either Value FacetValue)

-- use NewValue type?
createFacetValue :: ProgCounter -> NewValue -> NewValue -> NewValue
createFacetValue [] vH vL               = vH
createFacetValue ((k,True):rest)  vH vL = NewValue $ Right $ FacetV k (createFacetValue rest vH vL) vL
createFacetValue ((k,False):rest) vH vL = NewValue $ Right $ FacetV k vL (createFacetValue rest vH vL)

instance Eq FacetValue where
  FacetV p v1 v2 == FacetV q w1 w2   = p == q && v1 == w1 && v2 == w2

instance Eq NewValue where
  NewValue v1 == NewValue v2 = v1 == v2

instance Show FacetValue where
  show (FacetV p v1 v2) = "{" ++ show p ++ ", " ++ show v1 ++ ", " ++ show v2 ++ "}"

instance Show NewValue where
  show (NewValue v) = show v

type Principal = Int
type Branch    = (Principal,Bool)
type ProgCounter = [Branch] -- i13n

newtype ProgCounterT m a = ProgCounterT { aPCT :: (StateT ProgCounter m) a }
 deriving (Monad, MonadTrans)

instance (Typeable1Monad m) => Typeable1 (ProgCounterT m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "ProgCounterT")
                     [typeOf1 (undefined :: m ())]

instance MonadState s m => MonadState s (ProgCounterT m) where
  get = lift $ get
  put = lift . put
         
runPCT :: ProgCounterT m a -> ProgCounter -> m (a, ProgCounter)
runPCT (ProgCounterT pct) pc = runStateT pct pc

class MonadProgCounter m where
  getProgCounter :: m ProgCounter

instance Monad m => MonadProgCounter (ProgCounterT m) where
  getProgCounter = ProgCounterT $ StateT $ \pc -> return (pc, pc) 

type M' = AOT (ProgCounterT M) -- i13n
               
newInterp t e = interp # (t, e)


-- Require PC for Ref case
refPC :: Typeable1Monad m => RequirePC m (Term, Environment) b
refPC = RequirePC $ return (\ jp -> case unsafeCoerce jp of
                               (Jp _ _ ((Ref t, _))) -> return True
                               _ -> return False)

-- Another problem: the store handles Value but not NewValue

-- goRefAdv proceed args@((Ref t),e) =
--   do result@(Address addr) <- proceed args
--      progCounter <- getProgCounter
--      store <- get
--      let v = storeLookup addr store
--      let fv = (createFacetValue progCounter v Bottom)
--      put (storeReplace addr fv store)
--      return result        

-- i13n
runM' :: M' Value -> ProgCounter -> Store -> ((Value, ProgCounter),Store)
runM' m pc s = runIdentity (runStateT (runPCT (runAOT prog) pc) s)
 where prog = do -- deploy (aspect (pcAnd (pcCall interp) refPC) goRefAdv) -- i13n
                 -- others...
                 m

-- testing

-- use implicit parameters??

test :: Term -> Environment -> Store -> ProgCounter -> String
test t env store pc = show (runM' (newInterp t env) pc store)

testDefault t = test t [] []

-- Tests, we skip them for now!

term0 = (App (Lam "x" (Var "x")) (Con 10))
termBot = (App Bot (Con 1))
termStore = (Deref (App (Lam "x" (App (Lam "y" (Var "x"))
                                      (Assign (Var "x") (Con 2))))
                        (Ref (Con 1))))

termLet = (Let "x" (Con 1) (Var "x"))
termSeq = (Let "x" (Ref (Con 1))
           (Seq
            (Assign (Var "x") (Con 2))
            ((Deref (Var "x")))))

-- TO DO: it fails to construct facetTest1 because it tries to mix Term with NewTerm
-- assert facetTest1 == (Facet (1,True) (Con 42) (Con 24))
-- facetTest1 = (If (Facet 1 (Bol True) (Bol False)) (Con 42) (Con 24))

-- assert facetTest1 ==
facetTest2 = (Ref (Con 843))

facetTest3 = (Let "x" (Ref (Bol True))
              (Assign (Var "x") (Bol False)))

fentonTestRaw = (Let "x" (Ref (Bol True))
                 (Let "y" (Ref (Bol True))
                  (Let "z" (Ref (Bol True))
                   (Seq
                    (Seq
                     (If (Deref (Var "x"))
                      (Assign (Var "y") (Bol False))
                      Bot)
                     (If (Deref (Var "y"))
                      (Assign (Var "z") (Bol False))
                      Bot))
                    (Deref (Var "z"))))))

-- fentonTest = (Let "x" (Ref (Facet 1 (Bol True) Bot))
--               (Let "y" (Ref (Bol True))
--                (Let "z" (Ref (Bol True))
--                 (Seq
--                  (Seq
--                   (If (Deref (Var "x"))
--                    (Assign (Var "y") (Bol False))
--                    Bot)
--                   (If (Deref (Var "y"))
--                    (Assign (Var "z") (Bol False))
--                    Bot))
--                  (Deref (Var "z"))))))

-- let x = ref (<1 ? true : ⟂>) in (
--   let y = ref true in (
--     let z = ref true in (
--       if !x then y := false;  # y = <1 ? false : true>
--       if !y then z := false;  # z = <1 ? true : false>
--         !z)))
