{-# LANGUAGE RankNTypes,
             TemplateHaskell
  #-}

module AOP.Examples.LevelAgnostic where

import AOP.ExecutionLevels

type AOELT m = AOT (ELT m)
runAOELT c = flip runELT 0 $ runAOT c

levelAgnosticAdv = withView v where v = i `vcomp` o `vcomp` i

showTag = $newTag

showM ::  (Monad m, Show a) => Function a (m String)
showM = mkFunction (\a -> return (show a)) showTag


logAdv proceed a = do argStr <- showM # a
                      result <- proceed a
                      resStr <- down $ showM # result
                      tell ("Arg: " ++ argStr ++ " Result: " ++ resStr)
                      return result

agnosticLogAdv proceed a = do argStr <- showM # a
                              result <- proceed a
                              resStr <- showM # result
                              tell ("Arg: " ++ argStr ++ " Result: " ++ resStr)
                              return result

-- In the default semantics of AOP this programs loops, because logAdv is triggered infinitely
-- many times when it evaluates showM # result. However, with the semantics of execution levels
-- the program executes correctly.
type M = AOELT (WriterT String Identity)
runM c = runIdentity $ runWriterT $ runAOELT c

program :: Int -> (String, String)
program n = runM $ do
        deploy (aspect (pcCall (showM :: Function Int (M String)))
                       (levelAgnosticAdv agnosticLogAdv))
        showM # n
        