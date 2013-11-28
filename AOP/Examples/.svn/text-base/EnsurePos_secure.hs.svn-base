{-# LANGUAGE TemplateHaskell,
             ScopedTypeVariables
 #-}

import AOP.Default
import Debug.Trace
import AOP.Internal.AOTType

runProgram p pc = runIdentity (runAOT_s (EPC pc) p)

ensurePos proceed n = proceed (abs n)

sqrtTag = $newTag

sqrtM = mkFunction (\(n::Float) -> return (sqrt n)) sqrtTag

-- using an aspect
program n = do deploy (aspect (pcCall sqrtM) ensurePos)
               sqrtM # n

-- Examples

-- Returns 2 because the argument is positive. Advice does not trigger because calls to sqrtM are hidden
example1  = runProgram (program 4) (pcCall sqrtM)

-- Returns NaN because calls to sqrtM are hidden and the advice does not trigger
example2  = runProgram (program (-4)) (pcCall sqrtM)

-- As in example1, returns 2
example3 = runProgram (program 4) pcFalse

-- Returns 2 because the calls to sqrtM are visible to the aspect
example4 = runProgram (program (-4)) pcFalse

