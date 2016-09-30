{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Test.Tasty
import Data.Map as M
import Data.List as L
import Data.Vector as V
import Text.PrettyPrint.PrettyTable
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
data R01 =  R01 {r1_id::Int, nested_r::R0}
  deriving (Show, G.Generic)
data R3 =  R3 {r3_id::Int, nested_rlist::[R0]}
  deriving (Show, G.Generic)
data R4 = R4 {r4_id::Int, nested_rtuple::(R0,R0)}
  deriving (Show, G.Generic)

instance Tabilize R0
instance Tabilize R01
instance Tabilize R3
instance Tabilize R4


getR0 = R0 {test_string="Jack-somone"
           , test_integer=10
           , test_double=10.101
           , test_float=0.101021}

-- nested record
getR1 = R01 {r1_id=1001, nested_r=getR0}

-- nested record in list
getR3 = R3 {r3_id=1001, nested_rlist=[getR0, getR0]}
-- tested nested record in tuple 
getR4 = R4 {r4_id=1001, nested_rtuple=(getR0, getR0)}

--testVector = testCase "testVector" (
    -- do
      -- create Vector
      -- getBox
      -- inspect Box (rows and columns)
--    )

testList = testCase "testList"
  (
    do
      let b = (L.head . L.head) $ listToBox [getR0]
      assertEqual "check rows" (B.rows b) 1
      assertEqual "check cols" (B.cols b) 13
  )

testLists = testGroup "test printing lists" [
  testList
  ]

tests :: TestTree
tests = testGroup "Tests" [
  testLists
  ]

main = defaultMain tests