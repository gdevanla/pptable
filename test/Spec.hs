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
data R01 =  R01 {test_string::String,
               test_integer::Integer,
               test_float::Float,
               test_double::Double}
  deriving (Data, Show, G.Generic)
instance T.CellValueFormatter R01

data R02 =  R02 {r2_id::Int, nested_r::R01}
  deriving (Show, G.Generic, Data)
instance T.CellValueFormatter R02

data R03 =  R03 {r3_id::Int, nested_r02:: R02}
          deriving (Show, G.Generic, Data)

instance T.Tabulate R01 T.ExpandWhenNested
instance T.Tabulate R02 T.ExpandWhenNested
instance T.Tabulate R03 T.ExpandWhenNested

getR01 = R01 {test_string="Jack-Jack"
           , test_integer=10
           , test_double=10.101
           , test_float=0.101021}

getR02 = R02 {r2_id=10, nested_r=getR01}

getR03 = R03 {r3_id=20, nested_r02=getR02}

testList = testCase "testList"
  (
    do
      let records = Prelude.replicate 2 $ getR03
          rows = B.rows $ T.renderTable records
          cols = B.cols $ T.renderTable records
      assertEqual "row count" rows 5
      assertEqual "col count" cols 91
  )


testMap = testCase "testMap"
  (
    do
      let records = M.fromList [("key1", getR03), ("key2", getR03)]
          rows = B.rows $ T.renderTable records
          cols = B.cols $ T.renderTable records
      assertEqual "row count" rows 5
      assertEqual "col count" cols 100
  )

testVector = testCase "testVector"
  (
    do
      let records = V.fromList [getR03, getR03]
          rows = B.rows $ T.renderTable records
          cols = B.cols $ T.renderTable records
      assertEqual "row count" rows 5
      assertEqual "col count" cols 91
  )

tests :: TestTree
tests = testGroup "Tests" [
  testList,
  testMap,
  testVector
  ]

main = defaultMain tests
