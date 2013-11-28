{-# LANGUAGE TemplateHaskell,
             ScopedTypeVariables
 #-}

import AOP.Default
import Debug.Trace

runProgram p pc aenv = runIdentity (runAOT_sp (SP pc aenv) p)

ensurePos proceed n = proceed (abs n)

sqrtTag = $newTag

sqrtM = mkFunction (\(n::Float) -> return (sqrt n)) sqrtTag

asp = aspect (pcCall sqrtM) ensurePos

-- using an aspect
program n = do deploy asp
               sqrtM # n

-- Examples

-- Returns NaN because calls to sqrtM are hidden, thus the advice does not trigger
example1  = runProgram (program (-4)) (pcCall sqrtM) []

-- Returns 2 because advice triggers, since there are no hidden join points
example2 = runProgram (program (-4)) pcFalse []

-- Returns 2 because, even though calls to sqrtM are hidden, asp is a privileged aspect
example3  = runProgram (program (-4)) (pcCall sqrtM) [EAspect asp]

