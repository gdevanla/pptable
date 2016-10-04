{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module implements the default methods for Tabulate

module Text.PrettyPrint.Tabulate
    (
      Tabulate
    , Boxable
    , CellValueFormatter
    , ppTable
    
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

-- | Future change to support providing custom formatting functions
data TablizeValueFormat = T {floatValueFormat::Maybe (Float -> String),
                             stringValueFormat::Maybe (String -> String),
                             integerValueFormat::Maybe (Integer -> String),
                             intValueFormat::Maybe (Int -> String),
                             doubleValueFormat::Maybe (Double -> String)}

-- | Default TabulateValueFormat
getDefaultTabulateValueFormat = T {floatValueFormat=Nothing,
                                   stringValueFormat=Nothing,
                                   integerValueFormat=Nothing,
                                   intValueFormat=Nothing,
                                   doubleValueFormat=Nothing}


-- | The Generalized class that implements the print feature
--   for any type that derives Generics and Data
class GTabulate f where
  -- | Print with default style
  gprintTable :: f a -> [B.Box]

  -- | For future, will be able to print with provided style
  gprintTableWithStyle :: TablizeValueFormat -> f a -> [B.Box]

--- | The instance class for Unit type
instance GTabulate U1 where
  gprintTable _ = []
  gprintTableWithStyle _ _ = []

-- | Any records or product types
-- | Nested algebraic types are printed using their respective Show methods
instance (GTabulate a, GTabulate b) => GTabulate (a :*: b) where
    gprintTable (a :*: b) = gprintTable a ++ gprintTable b
    gprintTableWithStyle style (a :*: b) = gprintTableWithStyle style a ++ gprintTableWithStyle style b

-- | Sum types
instance (GTabulate a, GTabulate b) => GTabulate (a :+: b) where
    gprintTable (L1 x) = gprintTable x
    gprintTable (R1 x) = gprintTable x

    gprintTableWithStyle style (L1 x) = gprintTableWithStyle style x
    gprintTableWithStyle style (R1 x) = gprintTableWithStyle style x

-- | 
instance (GTabulate a) => GTabulate (M1 i c a) where
  gprintTable (M1 x) = gprintTable x
  gprintTableWithStyle style (M1 x) = gprintTableWithStyle style x


-- | The leaf method that actually creates the Boxes that will
-- be used to render the boxes as a table.
instance (CellValueFormatter a) => GTabulate (K1 i a) where
  gprintTable (K1 x) = [B.text $ ppFormatter x]
  gprintTableWithStyle style (K1 x) = [B.text $ ppFormatterWithStyle style x]


-- |  Class that implements formatting using printf.
--    Default instances for String, Char, Int, Integer, Double and Float
--    are provided. For types that are not an instance of this class
--    `show` is used.
class CellValueFormatter a where

  -- Function that can be implemented by each instance
  ppFormatter :: a -> String

  -- Future support for this signature will be added
  ppFormatterWithStyle :: TablizeValueFormat -> a -> String

  -- Default instance of function for types that do
  -- do not have their own instance
  default ppFormatter :: (Show a) => a -> String
  ppFormatter x =  show x

  -- Future support.
  default ppFormatterWithStyle :: (Show a) => TablizeValueFormat ->  a -> String
  ppFormatterWithStyle _ x =  "default_" ++ show x


instance CellValueFormatter Integer where
  ppFormatter x = printf "%d" x
  
  ppFormatterWithStyle style x = case integerValueFormat style of
    Just f -> f x
    Nothing -> ppFormatter x

instance CellValueFormatter Int where
  ppFormatter x = printf "%d" x

  ppFormatterWithStyle style x = case intValueFormat style of
    Just f -> f x
    Nothing -> ppFormatter x


instance CellValueFormatter Float where
  ppFormatter x = printf "%14.9g" x

  ppFormatterWithStyle style x = case floatValueFormat style of
    Just f -> f x
    Nothing -> ppFormatter x

instance CellValueFormatter String where
  ppFormatter x = printf "%s" x

  ppFormatterWithStyle style x = case stringValueFormat style of
    Just f -> f x
    Nothing -> ppFormatter x


instance CellValueFormatter Double where
  ppFormatter x = printf "%14.9g" x

  ppFormatterWithStyle style x = case doubleValueFormat style of
    Just f -> f x
    Nothing -> ppFormatter x

instance CellValueFormatter Bool

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

-- | Class that can be derived by a 'Traversable' to create
--   a list of 'Box' values and print as a Table.
--   Default instances for List, Map and Vector are already provided.
class Boxable b where
  toBox :: (Data a, G.Generic a, GTabulate(Rep a)) => b a ->  [[B.Box]]
  --toBoxWithStyle :: (Data a, G.Generic a, GTabulate(Rep a)) => TablizeValueFormat -> b a ->  [[B.Box]]
  
  printTable :: (Data a, G.Generic a, GTabulate(Rep a)) => b a -> IO ()
  --printTableWithStyle :: (Data a, G.Generic a, GTabulate(Rep a)) => TablizeValueFormat -> b a -> IO ()

instance Boxable [] where
  toBox a = case a of
    [] -> [[B.nullBox]]
    x:xs -> gprintTable (from x):toBox xs

  -- | Prints a "List" as a table. Called by "ppTable"
  -- | Need not be called directly
  printTable m = do
    let r = head $ m
    let header = constrFields . toConstr $ r
    let header_box = List.map (B.text) header
    B.printBox $ alignBox $ header_box:toBox m

  
instance Boxable V.Vector where
  toBox v = V.toList $ fmap (\x -> (gprintTable (from x))) v

  -- | Prints a "Vector" as a table. Called by "ppTable"
  -- | Need not be called directly
  printTable m = do
    let r = m V.! 0
    let header = constrFields . toConstr $ r
    let header_box = List.map (B.text) header
    B.printBox $ alignBox $ header_box:toBox m

instance (Show k) => Boxable (Map.Map k) where
  -- | Returns a Map of Boxed values
  toBox m =  Map.elems
              (Map.mapWithKey
               (\k v -> B.text (show k):gprintTable (from v))
               m)
  -- | Prints a "Map" as a table. Called by "ppTable"
  -- | Need not be called directly
  printTable m = do
    let r = head . Map.elems $ m
    let header = constrFields . toConstr $ r
    let header_box = "Key":List.map (B.text) header
    B.printBox $ alignBox $ header_box:toBox m

    
-- | The elements in a Traverserable should be an instance of Tabulate to be displayed in a tabular format
class  (Data a) => Tabulate a where

  -- | Generic function that will be provided by the GTabulate class.
  ppTable :: (Boxable f) => f a -> IO()
  --ppTableWithStyle :: TablizeValueFormat -> f a -> IO()

  default ppTable :: (Boxable f, G.Generic a, GTabulate (Rep a)) => f a -> IO ()
  ppTable x = printTable x

  --default ppTableWithStyle :: (Boxable f, G.Generic a, GTabulate (Rep a)) => TablizeValueFormat -> f a -> IO ()
  --ppTableWithStyle x = printTableWithStyle x

