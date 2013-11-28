{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances
 #-}

module AOP.Internal.THMacros (typepc', deployTC, deployCall, deployType, newTag) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta
import Language.Haskell.Meta.Utils
import Data.Unique
import Data.List 
import qualified Data.Set as Set

-- | Creates a unique integer, used to identify functions.
-- Each splice of this macro creates an integer value **only once**
-- So if used inside of a function, **it will always evaluate to the same value**
newTag :: Q Exp
newTag = do tag <- runIO newUnique
            lift $ hashUnique tag

deployTC :: String -> String -> Q Exp -> Q Exp
deployTC className funName qadv = _deployTCImpl tc fun qadv
                                         where tc = mkName className
                                               fun = mkName funName

_deployTCImpl :: Name -> Name -> Q Exp -> Q Exp
_deployTCImpl tc funname qadv = do
         instances <- getInstances tc
         let types = map instanceType instances
         ptyp <- instancePolymorphicType funname
         adv <- qadv
         annotated <- sequenceQ (map (annotateInstance funname adv) types)
         let annotatedExps = map fst annotated
         let annotatedAdvs = map snd annotated
         let pcs = map callPcExp' annotatedExps
         let finalList = zip pcs annotatedAdvs
         aspects <- sequenceQ (map (\(pc, adv) -> deployExp pc adv) finalList)
         return (DoE (map NoBindS aspects))

advExp :: Exp -> Type -> Exp
advExp adv ann = SigE adv (AppT (ConT (mkName "Advice")) ann)

callPcExp :: Name -> Exp
callPcExp name = appEStr' "pcCall" name


callPcExp' :: Exp -> Exp
callPcExp' exp = appEStr "pcCall" exp

typePcExp' :: Exp -> Exp
typePcExp' exp = appEStr "pcType" exp

appEStr :: String -> Exp -> Exp
appEStr s exp = AppE (VarE (mkName s)) exp


appEStr' :: String -> Name -> Exp
appEStr' s n = AppE (VarE (mkName s)) (VarE n)


deployExpQ :: Q Exp -> Q Exp -> Q Exp
deployExpQ qpc qadv = do
           pc <- qpc
           adv <- qadv
           return (AppE (VarE (mkName "deploy")) (AppE (AppE (VarE (mkName "aspect")) pc) adv))


deployExp :: Exp -> Exp -> Q Exp
deployExp pc adv = deployExpQ (return pc) (return adv)


getInstances :: Name -> Q [Dec]
getInstances typ = do
   ClassI _ instances <- reify typ
   return instances


-- the [Dec] part of InstanceD declarations are guaranteed to be empty, by reify.
-- this works only for type classes with one parameter
-- there are nested AppT constructors for MultiParamTypeClasses
instanceType :: Dec -> Type
instanceType (InstanceD ctx typ _) =
                case typ of
                     (AppT typ1 typ2) -> typ2
                     otherwise -> error "error in instanceToTypes"


showInstances :: Name -> Q Exp
showInstances typ = do
   ins <- getInstances typ
   return . LitE . StringL $ show (map instanceType ins)


-- Returns 'True' or 'False' exp
instanceOf :: Name -> Q Type -> Q Exp
instanceOf a b = do typ <- b
                    x <- isInstance a [typ]
                    lift x

annotateInstance :: Name -> Exp -> Type -> Q (Exp, Exp)
annotateInstance name adv typ = do
                 ClassOpI opname ptyp pclass _ <- reify name
                 varNames <- getVarNames name
                 ForallT _ _ newtyp <- return (applyT ptyp typ)
                 return (SigE (VarE opname) newtyp, adv)


getTypeVariables :: Type -> ([TyVarBndr], Cxt, Type)
getTypeVariables typ = case typ of
                 (ForallT vars cxt t) -> (vars, cxt, t)
                 otherwise -> error ("THMacros: Type is not polymorphic" ++ show typ)


instancePolymorphicType :: Name -> Q Type
instancePolymorphicType name = do
                        info <- reify name
                        case info of
                          ClassOpI _ ptyp _ _ -> return ptyp
                          VarI n t _ _ -> return t
                          otherwise -> error "Class Name or Var"


showPolyType :: Name -> Q Exp
showPolyType name = do
             ptyp <- instancePolymorphicType name             
             return . LitE . StringL $ show ptyp


getVarNames :: Name -> Q ([Name], Cxt, Type)
getVarNames name = do
            ptyp <- instancePolymorphicType name
            let (typVars, cxt, t) = getTypeVariables ptyp
            return (map toName typVars, cxt, t)

getConstrainedVariable :: Pred -> [Type]
getConstrainedVariable (ClassP n typs) = typs
getConstrainedVariable (EqualP _ _) = []


getVariableConstraintsList ::[Pred] -> Type -> (Type, [Name])
getVariableConstraintsList preds t = (t, concatMap (getVariableConstraints t) preds)


getVariableConstraints :: Type -> Pred -> [Name]
getVariableConstraints t (ClassP n typs) = if t `elem` typs then [n] else [] 
getVariableConstraints t (EqualP _ _) = []


getInstancesList :: [Name] -> [Q [Dec]]
getInstancesList (name:names) = if not (null names) 
                                   then getInstances name : getInstancesList names
                                   else [getInstances name]


getNameInstances (x:xs) = let classes = snd x 
                          in do
                             instances <- sequenceQ (getInstancesList classes)
                             if not (null xs)
                                then do restInstances <- getNameInstances xs
                                        return (instances : restInstances)
                                else return [instances]
getNameInstances [] = error "THMacros error 1"


getInstancesTypes (dec:[]) = [map (map instanceType) dec]
getInstancesTypes (dec:decs) = map (map instanceType) dec  : getInstancesTypes decs
getInstancesTypes [] = error "THMacros error 2"


getConstraintPermutations name = do
             (tyVar, cxt, t) <- getVarNames name
             let constrainedVars = nub (concatMap getConstrainedVariable cxt)
             let varsWithClasses = map (getVariableConstraintsList cxt) constrainedVars
             instances <- getNameInstances varsWithClasses
             let types = getInstancesTypes instances
             let sets = map (map Set.fromList) types
             let intersectedSets = map (\xs -> foldr Set.intersection (head xs) xs) sets
             let intersectedTypes = map Set.toList intersectedSets
             let sequenced  = sequence intersectedTypes
             let result = (constrainedVars, sequenced)
             return result


getAnnotatedConstraints name = do
             (tyVar, cxt, t) <- getVarNames name
             permutations <- getConstraintPermutations name
             let tyVars = fst permutations
             let firstPerm = head (snd permutations)
             let tyNames = map (\t -> case t of (VarT name) -> name) tyVars
             let substArgs = map (zip tyNames) (snd permutations)
             let newtypes = map (\arg -> substT arg [] t) substArgs
             let annotatedTypes = map (SigE (VarE name)) newtypes
             return (annotatedTypes, newtypes)

-- TO DO advice also has to be annotated!!
-- Otherwise safety is lost, e.g. Int vs Double with a function with constraint Num a.
-- Annotating advice requires named advice instead of [| ... |]
deployCall :: String -> Q Exp -> Q Exp
deployCall name qadv = do
           let funname = mkName name
           adv <- qadv
           annotatedTypes <- getAnnotatedConstraints funname
           let pcs = map callPcExp' (fst annotatedTypes)
           --let advs = map (advExp adv) (snd annotatedTypes)
           --let annotatedAspects = zipWith deployExp pcs advs
           --aspects <- sequenceQ annotatedAspects
           aspects <- sequenceQ (map (\pc -> deployExp pc adv) pcs)
           return (DoE (map NoBindS aspects))


deployType :: String -> Q Exp -> Q Exp
deployType name qadv = do
           let funname = mkName name
           adv <- qadv
           annotatedTypes <- getAnnotatedConstraints funname
           let pcs = map typePcExp' (fst annotatedTypes)
           aspects <- sequenceQ (map (\pc -> deployExp pc adv) pcs)
           return (DoE (map NoBindS aspects))


typepc' :: String -> Q Exp
typepc' ts = case parseType ts of
               (Left s) -> error ("Error parsing type " ++ ts)
               (Right t) -> return (AppE (VarE (mkName "pcType")) (SigE (VarE (mkName "undefined")) t))

typeUndef :: Q Type -> Q Exp
typeUndef qt = do t <- qt
                  return $ SigE (VarE (mkName "undefined")) t
