{-# LANGUAGE DeriveGeneric #-} -- Remove this
{-# LANGUAGE DeriveDataTypeable #-} -- Remove this
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-#LANGUAGE ScopedTypeVariables #-}


-- | Module implements the default methods for Tabulate

module Text.PrettyPrint.Tabulate
    -- (
    --   Tabulate(..)
    -- , Boxable(..)
    -- , CellValueFormatter
    -- , (...)
    -- )
where

import Data.Maybe
import Data.Data
import Data.Tree
import Data.Typeable
import Data.Generics.Aliases
import GHC.Generics as G
import GHC.Show
import qualified Data.Map as Map
import qualified Text.PrettyPrint.Boxes as B
import qualified Data.List as List
import qualified Data.List as L
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

data Tag = Constr | Fields | Values deriving (Show)

class GRecordMeta f where
  toTree:: f a -> [Tree String]

instance GRecordMeta U1 where
    toTree U1 = []

instance (GRecordMeta (a), GRecordMeta (b)) => GRecordMeta (a :*: b) where
  toTree (x :*: y) = (toTree x)  ++ (toTree y)

instance (GRecordMeta (a), GRecordMeta (b)) => GRecordMeta (a :+: b) where
  toTree x = toTree x

instance (GRecordMeta a, Selector s) => GRecordMeta (M1 S s a) where
  toTree a = [Node (selName a) $ toTree (unM1 a)] where

instance (GRecordMeta a, Constructor c) => GRecordMeta (M1 C c a) where
  -- we don't want to build node for constructor
  --toTree a = [Node (conName a) $ toTree (unM1 a)]
  toTree a = toTree (unM1 a)

instance (GRecordMeta a) => GRecordMeta (M1 D c a) where
  toTree (M1 x) = toTree x

instance (CellValueFormatter a, Data a, RecordMeta a) => GRecordMeta (K1 i a) where
  --toTree x = [Node (show (unK1 x)) (toTree' $ unK1 x)]
  toTree x = toTree' $ unK1 x

data HTrue
data HFalse
class Tabulate a flag | a->flag where {}

--instance TypeCast flag HFalse => Tabulate a flag

instance (flag ~ HFalse) => Tabulate a flag

class RecordMeta a where
  toTree':: a -> [Tree String]

instance (Tabulate a flag, RecordMeta' flag a) => RecordMeta a where
  toTree' = toTree'' (undefined::proxy flag)

class RecordMeta' flag a where
  toTree'':: proxy flag -> a -> [Tree String]

instance (G.Generic a, GRecordMeta (Rep a)) => RecordMeta' HTrue a where
  toTree'' _ a = toTree (G.from a)

instance (CellValueFormatter a) => RecordMeta' HFalse a where
  toTree'' _ a = [Node (ppFormatter a) []]


printPath :: Tree a -> [[a]]
printPath (Node r []) = [[r]]
printPath (Node r f) = let
  p = [r:x | x <- (L.concatMap printPath f)]
  in
  p

fillPath paths = stripped_paths where
  depth = L.maximum $ L.map L.length paths
  diff = L.map (\p -> depth - (L.length p)) paths
  new_paths = L.map (\(p,d) ->  p ++ L.replicate d "-") $ L.zip paths diff
  stripped_paths = [xs | x:xs <- new_paths]

countLeaves :: Tree a -> Tree (Int, a)
countLeaves (Node r f) = case f of
  [] -> Node (1, r) []
  x -> countLeaves' x where
    countLeaves' x  = let
      count_leaves = fmap countLeaves x
      level_count = Prelude.foldr (\(Node (c, a) _) b -> c + b) 0 count_leaves
      in
      Node (level_count, r) count_leaves

trimTree (Node r f) = trimLeaves r f

trimLeaves r f = Node r (trimLeaves' f) where
  trimLeaves' f =
    let result = fmap trimLeaves'' f where
          trimLeaves'' (Node r' f') = let
            result' = case f' of
              [] -> Nothing
              _ -> Just $ trimLeaves r' f'
            in
            result'
    in
      catMaybes result

getLeaves :: (CellValueFormatter a) => Tree a -> [String]
getLeaves (Node r f) = case f of
  [] -> [(ppFormatter r)]
  _ -> foldMap getLeaves f

data T = C1 { aInt::Int, aString::String} deriving (Data, Typeable, Show,G.Generic)
data T1 = C2 { t1:: T, bInt::Float, bString::String} deriving (Data, Typeable, Show, G.Generic)

c1 = C1 10 "record_c1fdsafaf"
c2 = C2 c1 100.12121 "record_c2"
c3 = C2 c1 10010101.12111 "record_c2fdsafdsafsafdsafasfa"
c4 = C2 c1 1001010010101.12121 "r"

instance Tabulate T HTrue
instance Tabulate T1 HTrue
instance CellValueFormatter T

data R2 = R2 {a::Maybe Integer} deriving (G.Generic, Show)
data R3 = R3 {r31::Maybe Integer, r32::String} deriving (G.Generic, Show)
tr =  Node "root" (toTree . G.from $ c2)
r2 = Node "root" (toTree . G.from $ (R2 (Just 10)))
r3 = Node "root" (toTree . G.from $ (R3 (Just 10) "r3_string"))

showTree :: (Show a) => Tree a -> Tree String
showTree (Node r f) = case f of
  [] -> Node (show r) []
  x -> showTree' x where
    showTree' x = let
      show_children = fmap showTree x
      in
      Node (show r) show_children

createBoxedHeaders :: [[String]] -> [B.Box]
createBoxedHeaders paths = boxes where
  boxes = L.map wrapWithBox paths
  wrapWithBox p = B.vsep 0 B.top $ L.map B.text p


-- TODO: Consolidate these 2 methods
ppRecords :: (GRecordMeta (Rep a), G.Generic a) => [a] -> IO ()
ppRecords recs = result where
  tr =  fmap (\a -> Node "root" $ (toTree . G.from $ a)) recs
  headers =  createBoxedHeaders . fillPath . printPath . trimTree . L.head $ tr
  horizontal_boxes =  fmap (fmap  B.text) $ fmap getLeaves tr
  cols = fmap (B.vsep 0 B.top) $ L.transpose horizontal_boxes
  result = B.printBox $ B.hsep 5 B.top $ fmap (\(a, b) -> B.vsep 0 B.top $ [a, b]) $ L.zip headers cols

ppRecordsWithIndex :: (CellValueFormatter k, GRecordMeta (Rep a), G.Generic a) => (Map.Map k a) -> IO ()
ppRecordsWithIndex recs = result where
  tr =  fmap (\a -> Node "root" $ (toTree . G.from $ a)) $ Map.elems recs
  headers =  createBoxedHeaders . fillPath . printPath . trimTree . L.head $ tr
  horizontal_boxes =  fmap (fmap  B.text) $ fmap getLeaves tr
  cols = fmap (B.vsep 0 B.top) $ L.transpose horizontal_boxes
  -- build index
  header_length = L.length . L.head . fillPath . printPath . trimTree . L.head $ tr
  index_col = (L.replicate header_length "-" ) ++  (L.map ppFormatter $ Map.keys recs)
  index_box = B.vsep 0 B.top $ L.map B.text index_col
  data_box = fmap (\(a, b) -> B.vsep 0 B.top $ [a, b]) $ L.zip headers cols
  result = B.printBox $ B.hsep 5 B.top $ index_box:data_box

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

instance (Show a, CellValueFormatter a) => CellValueFormatter (Maybe a)

class Boxable b where
  -- toBox :: (Data a, G.Generic a, GRecordMeta(Rep a)) => b a ->  [[B.Box]]
  -- toBoxWithStyle :: (Data a, G.Generic a, GTabulate(Rep a)) => TablizeValueFormat -> b a ->  [[B.Box]]

  printTable :: (G.Generic a, GRecordMeta (Rep a)) => b a -> IO ()
  --printTableWithStyle :: (Data a, G.Generic a, GTabulate(Rep a)) => TablizeValueFormat -> b a -> IO ()

instance Boxable [] where
  -- | Prints a "List" as a table. Called by "ppTable"
  -- | Need not be called directly
  printTable m = ppRecords m

instance Boxable V.Vector where
  -- | Prints a "Vector" as a table. Called by "ppTable"
  -- | Need not be called directly
  printTable m = ppRecords $ V.toList m  --TODO: switch this to Vector


instance (CellValueFormatter k) => Boxable (Map.Map k) where

  -- | Prints a "Map" as a table. Called by "ppTable"
  -- | Need not be called directly
  printTable m = ppRecordsWithIndex m
