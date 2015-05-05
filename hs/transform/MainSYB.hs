{-# LANGUAGE FlexibleInstances #-}

import System.Environment
import System.IO
import Haskull.Language.Haskell.Parser
import Haskull.Language.Haskell.Syntax
import Haskull.Language.Haskell.Pretty

import Data.Generics

-- TODO: Follow translation from Open Data Types and Open Functions
-- more closely

-- TODO: use Transform for defining the syntax extension (bootstrap).

-- TODO: Should transport imports and LANGUAGE pragmas
-- Does the order if import matters?

-- TODO: Should translate multiple source files into one

-- FIXME: If an extended data type had a typeclass instance, how
-- should the new constructor be added to the cases of the typeclass
-- definition?


translate :: HsModule -> HsModule
translate = everywhere (mkT rewrite)

rewrite :: [HsDecl] -> [HsDecl]
rewrite = removeExtend . regroupFuns . removeExtendData . regroupTerms

regroupFuns :: [HsDecl] -> [HsDecl]
regroupFuns ds = map (mergeFunWithExtend (filter isExtend ds)) ds

mergeFunWithExtend :: [HsDecl] -> HsDecl -> HsDecl
mergeFunWithExtend es (HsFunBind ms@((HsMatch _ name _ _ _):_)) =
    HsFunBind (ms ++ (matchesWithName name es))
mergeFunWithExtend _ other = other

matchesWithName :: HsName -> [HsDecl] -> [HsMatch]
matchesWithName name es = concat (map projectMatches (filter (\e -> name == projectName e) es))

projectMatches :: HsDecl -> [HsMatch]
projectMatches (HsExtendDecl _ _ decls) = concat (map projectMatches decls)
projectMatches (HsFunBind ms) = ms

removeExtend :: [HsDecl] -> [HsDecl]
removeExtend = filter (not . isExtend)

regroupTerms :: [HsDecl] -> [HsDecl]
regroupTerms ds = map (mergeOpenWithExtend (filter isExtendData ds)) ds

removeExtendData :: [HsDecl] -> [HsDecl]
removeExtendData = filter (not . isExtendData)

mergeOpenWithExtend :: [HsDecl] -> HsDecl -> HsDecl
mergeOpenWithExtend eds (HsOpenDataDecl srcLoc ctxt name names conDecls qNames) =
    HsDataDecl srcLoc ctxt name names newDecls qNames
    where newDecls = conDecls ++ matchingDecls
          matchingDecls = concat (map projectConDecls (filter (\ed -> name == projectName ed) eds))
mergeOpenWithExtend _ other = other

projectName :: HsDecl -> HsName
projectName (HsExtendDataDecl _ _ name _ _ _) = name
projectName (HsExtendDecl _ name _) = name

projectConDecls :: HsDecl -> [HsConDecl]
projectConDecls (HsExtendDataDecl _ _ _ _ conDecls _) = conDecls

isExtendData :: HsDecl -> Bool
isExtendData (HsExtendDataDecl _ _ _ _ _ _) = True
isExtendData _ = False

isExtend :: HsDecl -> Bool
isExtend (HsExtendDecl _ _ _) = True
isExtend _ = False

toAST :: FilePath -> IO (ParseResult HsModule)
toAST f = openFile f ReadMode >>= \handle ->
          hGetContents handle >>= \content ->
          return (parseModule content)

main :: IO ()
main = do
  (filename:_) <- getArgs
  parsed <- toAST filename
  case parsed of
    (ParseOk ast) -> putStrLn $ prettyPrint (translate ast) --print ast
    (ParseFailed loc err) -> putStrLn err >> putStrLn (show loc)
