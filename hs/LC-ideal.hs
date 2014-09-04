{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DeriveDataTypeable #-}

import "mzv" Control.Monad.State
--import Control.Monad.Identity
import "mzv" Control.Monad.Mask
--import Control.Monad.Views
-- import Control.Monad.State
-- import Control.Monad.Identity
import "effective-aspects-mzv" AOP.Default
-- import Data.Typeable

import qualified LCS as S

type Principal = Int

data Value = Raw S.Value
           | Facet Principal Value Value
  deriving Show
--    deriving Typeable

type Branch = (Principal,Bool)
type ProgCounter = [Branch]

data PCTag = PCTag

-- instance Eq Value where
--   FacetV p v1 v2 == FacetV q w1 w2   = p == q && v1 == w1 && v2 == w2

-- instance Show Value where
--   show (Facet p v1 v2) = "{" ++ show p ++ ", " ++ show v1 ++ ", " ++ show v2 ++ "}"

-- createFacetValue :: ProgCounter -> Value -> Value -> Value
-- createFacetValue [] vH vL                 = vH
-- createFacetValue ((k,True):rest)  vH vL = FacetV k (createFacetValue rest vH vL) vL
-- createFacetValue ((k,False):rest) vH vL = FacetV k vL (createFacetValue rest vH vL)

type M = TStateT PCTag ProgCounter S.M

runM :: M Value -> S.Environment -> S.Store -> ProgCounter -> (((Value, ProgCounter), S.Environment), S.Store)
runM m env store pc = runIdentity $ runTStateT store (runTStateT env (runTStateT pc prog))
    where prog = do --deploy (aspect (pcCall goRef) goRefAdv)
                    m

-- runM :: M Value -> ProgCounter -> S.Store -> ((Value, ProgCounter), S.Store)
-- runM m pc s = runIdentity (runStateT (runStateT (runAOT prog) pc) s)
--  where prog = do deploy (aspect (pcCall goRef) goRefAdv)
--                  deploy (aspect (pcCall goDeref) goDerefAdv)
--                  deploy (aspect (pcCall goAssign) goAssignAdv)
--                  deploy (aspect (pcCall goApply) goApplyAdv)
--                  m

ret v = return (v, return (), return (), return ())

interp :: forall m e s p.
              (Monad m,
               TWith S.EnvTag e m, MonadState S.Environment e,
               TWith S.StoreTag s m, MonadState S.Store s,
               TWith PCTag p m, MonadState ProgCounter p)
          => S.Term -> m (Value, e (), s (), p ())
interp (S.Ref t) =
    do (v, _, _) <- S.interp (S.Ref t) :: m (S.Value, e (), s ())
       ret $ Raw v


-- extend interp (App t u) e =
--     do f <- interp t e
--        a <- interp u e
--        apply f a

-- around interp (Ref t) e =
--     do result@(Address addr) <- proceed args
--        progCounter <- getProgCounter
--        store <- getStore
--        let v = storeLookup addr store
--        let fv = (createFacetValue progCounter v Bottom)
--        putStore (storeReplace addr fv store)
--        return result

-- -- helpers

-- ------------------------------ DEREF
-- extend deref (FacetV p vH vL) =
--     do progCounter <- getProgCounter
--        goDerefAdvHelper t progCounter

-- derefHelper :: Value -> ProgCounter -> M Value
-- derefHelper (FacetV p vH vL) progCounter =
--     case (p,True) `elem` progCounter of
--     True -> deref vH
--     False -> case (p,False) `elem` progCounter of
--                True -> deref vL
--                False -> do vH' <- deref vH
--                            vL' <- deref vL
--                            return (createFacetValue [(p,True)] vH' vL')

-- ------------------------------ ASSIGN
-- around assign left right =

-- -- i13n
-- goAssignAdv proceed args@(left, right) =
--     case left of
--       (FacetV p vH vL) -> do progCounter <- get
--                              put ((p,True):progCounter)
--                              r1 <- assign vH right
--                              put ((p,False):progCounter)
--                              r2 <- assign vL right
--                              put progCounter
--                              return right

--       (Address a) -> do store <- lift $ lift $ get
--                         progCounter <- get
--                         let v = storeLookup a store
--                         let fv = createFacetValue progCounter right v
--                         (lift . lift . put) (storeReplace a fv store)
--                         return right

--       otherwise -> proceed args


-- ------------------------------ APPLY
-- apply :: Value -> Value -> M Value
-- apply v1 v2 = goApply # (v1, v2) -- i13n

-- goApply :: (Value, Value) -> M Value
-- goApply (v1, v2) =
--     case v1 of
--       Bottom -> return Bottom

--       (Closure x body env) -> interp body ((x,v2):env)

-- goApplyAdv proceed args@(v1, v2) =
--     case v1 of
--       (FacetV p vH vL) -> do progCounter <- get
--                              if (p,True) `elem` progCounter
--                              then apply vH v2
--                              else
--                                  if (p,False) `elem` progCounter
--                                  then apply vL v2
--                                  else do put ((p,True):progCounter)
--                                          vH' <- apply vH v2
--                                          put ((p,False):progCounter)
--                                          vL' <- apply vL v2
--                                          put progCounter
--                                          return (createFacetValue [(p,True)] vH' vL')

--       otherwise -> proceed args

-- -- other helpers
-- envLookup :: Name -> Environment -> Value
-- envLookup x env = case (lookup x env) of
--                     Just v -> v
--                     Nothing -> (Error ("unbound " ++ show x))

-- storeLookup :: Address -> Store -> Value
-- storeLookup a store = case (lookup a store) of
--                         Just v -> v
--                         Nothing -> (Error ("not in store " ++ show a))

-- storeReplace :: Address -> Value -> Store -> Store
-- storeReplace a v [] = []
-- storeReplace a v ((b,w):s) = if a == b then ((a,v):s)
--                              else ((b,w):(storeReplace a v s))

-- -- testing

-- -- use implicit parameters??

interpWrap :: forall m e s p.
              (Monad m,
               TWith S.EnvTag e m, MonadState S.Environment e,
               TWith S.StoreTag s m, MonadState S.Store s,
               TWith PCTag p m, MonadState ProgCounter p)
              => S.Term -> m Value
interpWrap t = do (v, _, _, _) <- interp t :: m (Value, e (), s (), p ())
                  return v

test t env store pc = print $ runM (interpWrap t) env store pc

testDefault t = test t [] [] []

-- test :: Term -> Environment -> ProgCounter -> Store -> String
-- test t env pc store = show (runM (interp t env) pc store)

-- testDefault t = test t [] [] []

-- -- i13n
-- testWithPC :: Term -> ProgCounter -> String
-- testWithPC t pc = show (runM (interp t []) pc [])

-- -- i13n
-- runWithPC t pc = runM (interp t []) pc []

-- -- Tests, we skip them for now!

-- term0 = (App (Lam "x" (Var "x")) (Con 10))
-- termBot = (App Bot (Con 1))
-- termStore = (Deref (App (Lam "x" (App (Lam "y" (Var "x"))
--                                       (Assign (Var "x") (Con 2))))
--                         (Ref (Con 1))))

-- termLet = (Let "x" (Con 1) (Var "x"))
-- termSeq = (Let "x" (Ref (Con 1))
--            (Seq
--             (Assign (Var "x") (Con 2))
--             ((Deref (Var "x")))))


-- -- assert facetTest1 == (Facet (1,True) (Con 42) (Con 24))
-- facetTest1 = (If (Facet 1 (Bol True) (Bol False)) (Con 42) (Con 24))

-- -- assert facetTest1 ==
-- facetTest2 = (Ref (Con 843))

-- facetTest3 = (Let "x" (Ref (Bol True))
--               (Assign (Var "x") (Bol False)))

-- fentonTestRaw = (Let "x" (Ref (Bol True))
--                  (Let "y" (Ref (Bol True))
--                   (Let "z" (Ref (Bol True))
--                    (Seq
--                     (Seq
--                      (If (Deref (Var "x"))
--                       (Assign (Var "y") (Bol False))
--                       Bot)
--                      (If (Deref (Var "y"))
--                       (Assign (Var "z") (Bol False))
--                       Bot))
--                     (Deref (Var "z"))))))

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

-- -- let x = ref (<1 ? true : ⟂>) in (
-- --   let y = ref true in (
-- --     let z = ref true in (
-- --       if !x then y := false;  # y = <1 ? false : true>
-- --       if !y then z := false;  # z = <1 ? true : false>
-- --         !z)))
