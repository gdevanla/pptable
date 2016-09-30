{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstrainedClassMethods #-}

-- | Module implements the default methods for Tabilize

module Text.PrettyPrint.Tabilize
    (
      
      Tabilize
    , printList
    , printMap
    , printVector
    
    , listToBox
    , mapToBox
    , vectorToBox

    )where

import Data.Data
import Data.Typeable
import Data.Generics.Aliases
import GHC.Generics as G
import GHC.Show
import qualified Data.Map as Map
import qualified Text.PrettyPrint.Boxes as B
import qualified Data.List as List
import Text.Printf
import qualified Data.Vector as V



-- | The Generalized class that provides the print
-- | functionality for any type that derives Generics and Data
class GTabilize f where
  gprintTable :: f a -> [B.Box]

--- | The instance class for Unit type
instance GTabilize U1 where
  gprintTable _ = []

-- | Any records or product types
-- | Nested algebraic types are printed using their respective Show methods
instance (GTabilize a, GTabilize b) => GTabilize (a :*: b) where
    gprintTable (a :*: b) = gprintTable a ++ gprintTable b

-- | Sum types
instance (GTabilize a, GTabilize b) => GTabilize (a :+: b) where
    gprintTable (L1 x) = gprintTable x
    gprintTable (R1 x) = gprintTable x

-- | 
instance (GTabilize a) => GTabilize (M1 i c a) where
  gprintTable (M1 x) = gprintTable x


-- | The leaf method that actually creates the Boxes that will
-- | be used to render the boxes as a table.
instance (Data a, Show a) => GTabilize (K1 i a) where
  gprintTable (K1 x) = createBox x

-- | Create the B.Box around the provided value
-- | If the value is an algebraic type the algebraic types
-- | Show method is used. All values have to be an instance of
-- | Data. This dependency is there to support identify Numeric
-- | values for right-aligning those values
createBox :: (Data x, Show x) => x -> [B.Box]
createBox x
  | isBool = [B.text $ show x]
  | isNumeric = [B.text $ printf "%10s" . show $ x]
  | otherwise = [B.text $ show x]
  where
    isBool = get_type == "Bool"
    isNumeric = get_type == "Integer" || get_type == "Double" || get_type == "Float"
    get_type = tyConName . typeRepTyCon . typeOf $ x

-- | Perform the final alignment of all
-- | generated [[B.Box]]
alignBox :: [[B.Box]] -> B.Box
alignBox b = B.hsep 5 B.left cols
  where
    cols = List.map (B.vcat B.left) $ List.transpose b

-- | Helper method that detects an algebraic data type
isAlgRepConstr t = case dataTypeRep . dataTypeOf $ t of
  AlgRep [_] -> True
  _ -> False

-- | Specialized class that provides default methods
-- methods to print List, Map or Vector values as a
-- pretty table
class (Data a) => Tabilize a where
  -- | Return a list of values wrapped in a Box. Each entry in input is assumed to be a
  -- list of records keyed by any data type. The first entry in the
  -- list of values will be used to infer the names of fields
  listToBox :: [a] -> [[B.Box]]

  -- | Return a list of values wrapped in Box. Each entry in input is assumed to be a
  -- list of records keyed by any data type. The first entry in the
  -- list of values will be used to infer the names of fields
  mapToBox ::(Show b) => Map.Map b a -> [[B.Box]]

  
  -- | Return a list of values wrapped in Box. Each entry in input is assumed to be a
  -- list of records keyed by any data type. The first entry in the
  -- list of values will be used to infer the names of fields
  vectorToBox :: V.Vector a -> [[B.Box]]

  default listToBox :: (G.Generic a, GTabilize (Rep a)) => [a] -> [[B.Box]]
  listToBox a = case a of
    [] -> [[B.nullBox]]
    x:xs -> gprintTable (from x):listToBox xs

  
  default mapToBox :: (G.Generic a, GTabilize (Rep a), Show b) => Map.Map b a -> [[B.Box]]
  mapToBox m =  Map.elems
                (Map.mapWithKey
                 (\k v -> B.text (show k):gprintTable (from v))
                 m)

  default vectorToBox :: (G.Generic a, GTabilize (Rep a)) => V.Vector a -> [[B.Box]]
  vectorToBox v = V.toList $ fmap (\x -> (gprintTable (from x))) v

  -- |
  -- > import qualified Data.Map as M
  -- > -- declare a Map
  -- > data Portfolio = M.Map String Stock
  -- > Add the Stock values we create
  -- > let p = M.fromList [("YHOO", yahoo), ("GOOG", google), ("AMZN" amazon)]
  -- >
  -- > printMap p
  -- >
  -- > Key        ticker     price          marketCap
  -- > "amzn"     "AMZN"     799.161717      3.7886e11
  -- > "goog"     "GOOG"     774.210101      5.3209e11
  -- > "yhoo"     "YHOO"     42.2910101         4.0e10
  printMap :: (Show b) => Map.Map b a -> IO ()
  printMap m = do
    let r = head . Map.elems $ m
    let header = constrFields . toConstr $ r
    let header_box = "Key":List.map (B.text) header
    B.printBox $ alignBox $ header_box:mapToBox m

  -- |
  -- > -- List of records
  -- > let tickers = [yahoo, google, amazon]
  -- > 
  -- > printList tickers
  -- > 
  -- > ticker     price          marketCap
  -- > "YHOO"     42.2910101         4.0e10
  -- > "GOOG"     774.210101      5.3209e11
  -- > "AMZN"     799.161717      3.7886e11
  printList :: [a] -> IO ()
  printList m = do
    let r = head $ m
    let header = constrFields . toConstr $ r
    let header_box = List.map (B.text) header
    B.printBox $ alignBox $ header_box:listToBox m

  -- |
  -- > import qualified Data.Vector as V
  -- > -- Vector of records
  -- > let tickers = V.fromList [yahoo, google, amazon]
  -- > 
  -- > printVector tickers
  -- > 
  -- > ticker     price          marketCap
  -- > "YHOO"     42.2910101         4.0e10
  -- > "GOOG"     774.210101      5.3209e11
  -- > "AMZN"     799.161717      3.7886e11
  printVector :: V.Vector a -> IO ()
  printVector m = do
    let r = m V.! 0
    let header = constrFields . toConstr $ r
    let header_box = List.map (B.text) header
    B.printBox $ alignBox $ header_box:vectorToBox m

  -- print :: (Functor f) => f a -> f [B.Box]
  -- default ppTabFL :: (G.Generic a, Tabilize (Rep a), Functor f) => (f a) -> f [B.Box]
  -- ppTabFL f = fmap (\x -> (gprintTable (from x))) f

