{-# LANGUAGE TemplateHaskell,
             NoMonomorphismRestriction
 #-}

import AOP.Default
import Debug.Trace

runProgram p = runIdentity (runAOT p)

ensurePos proceed n = proceed (abs n)

-- Monadic version of sqrt
-- To use StableNames successfully we need to annotate the type of sqrtM
sqrtM :: Float -> AOT Identity Float
sqrtM n = return (sqrt n)

-- using an aspect
program n = do deploy (aspect (pcCall sqrtM) ensurePos)
               sqrtM # n

-- Examples
example1 = runProgram (program 4)
example2 = runProgram (program (-4))

{-
To avoid the type annotations we use our notion of tagged functions
Now, in this example we need the NoMonomorphismRestriction to avoid 
to put a generic annotation in sqrtM'. 

The MR states that some terms are inferred to be monomorphic, unless
explicit annotations are given.

In our case we run into the MR because we are using an inline
function definition (\n -> return (sqrt n)).
-}

sqrtTag = $newTag

sqrtM' :: (Monad m) => Function Float (m Float)
sqrtM' = mkFunction (\n -> return (sqrt n)) sqrtTag

program' n = do deploy (aspect (pcCall sqrtM') ensurePos)
                sqrtM' # n

-- Examples
example3 = runProgram (program' 4)
example4 = runProgram (program' (-4))