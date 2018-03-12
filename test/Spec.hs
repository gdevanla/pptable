{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses#-}

import Data.Map as M
import Data.List as L
import Data.Vector as V
import qualified Text.PrettyPrint.Tabulate as T
import GHC.Generics as G
import Data.Data
import Text.PrettyPrint.Boxes as B

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

-- R0 has to derive from Data, since
-- it will be nested
data R0 =  R0 {test_string::String,
               test_integer::Integer,
               test_float::Float,
               test_double::Double}
  deriving (Data, Show, G.Generic)
-- data R01 =  R01 {r1_id::Int, nested_r::R0}
--   deriving (Show, G.Generic, Data)
-- data R3 =  R3 {r3_id::Int, nested_rlist::[R0]}
--   deriving (Show, G.Generic, Data)
-- data R4 = R4 {r4_id::Int, nested_rtuple::(R0,R0)}
--   deriving (Show, G.Generic, Data)

instance T.Tabulate R0 T.ExpandWhenNested
-- instance T.Tabulate R01
-- instance T.Tabulate R3
-- instance T.Tabulate R4

getR0 = R0 {test_string="Jack-somone"
           , test_integer=10
           , test_double=10.101
           , test_float=0.101021}

testList = testCase "testList"
  (
    do
      assertEqual "test" 1 1
  )

testLists = testGroup "test printing lists" [
  testList
  ]

tests :: TestTree
tests = testGroup "Tests" [
  testLists
  ]

main = defaultMain tests
