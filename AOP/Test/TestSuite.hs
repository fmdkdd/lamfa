{-# LANGUAGE TemplateHaskell #-}


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import AOP.Test.AOT as AOT
import AOP.Test.Cflow as Cflow
import AOP.Test.ELT as ELT
import AOP.Test.Membranes as Membranes
import AOP.Test.NIAOT as NIAOT
-- import AOP.Test.SecureAOT as SecureAOT
import AOP.Test.SecurePrivilegedAOT as SP
import AOP.Test.Macros as Macros

main = defaultMain testSuite

testSuite =  [ AOT.tests,
               Cflow.tests,
               ELT.tests,
               Membranes.tests,
               NIAOT.tests,
               -- SecureAOT.tests,
               SP.tests,
               Macros.tests
         ]

