{-# LANGUAGE FlexibleInstances #-}

import System.IO
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty

toAST :: FilePath -> IO (ParseResult HsModule)
toAST f = do
  handle <- openFile f ReadMode
  content <- hGetContents handle
  return $ parseModule content

class Merge a where
    merge :: a -> a -> a

instance Merge (ParseResult HsModule) where
    merge a@(ParseFailed err _) _ = a
    merge _ a@(ParseFailed err _) = a
    merge (ParseOk baseAST) (ParseOk extAST) = ParseOk (merge baseAST extAST)

instance Merge HsModule where
    merge (HsModule _ _ _ _ baseDecls)
          (HsModule pos m mbExports imp extDecls)
          = HsModule pos m mbExports imp $ mergeHelper baseDecls extDecls

mergeHelper :: Merge a => [a] -> [a] -> [a]
mergeHelper xs ys = map reduceMerge xs
    where reduceMerge x = foldl merge x ys

instance Merge HsDecl where
    merge base@(HsDataDecl src ctxt name names conDecl qNames)
          ext@(HsDataDecl _ _ extName _ extConDecl _)
          = if (name == extName)
            then HsDataDecl src ctxt name names (conDecl ++ extConDecl) qNames
            else base
    merge base@(HsFunBind baseMatches)
          ext@(HsFunBind extMatches)
          = HsFunBind (baseMatches ++ extMatches)
    merge base _ = base

main = do
  baseResult <- toAST "Base.hs"
  extResult <- toAST "Extension.hs"
  case merge baseResult extResult of
    (ParseOk ast) -> putStrLn $ prettyPrint ast --print ast
    (ParseFailed err _) -> print err
