{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | This module features methods that can be used to print values contained
-- inside a list \/ a map \/ or a vector
-- in a tabular format.

module Text.PrettyPrint.PrettyTable
    (
      -- ** Example of output
      -- $outputexample

      -- ** Basic Steps
      -- $steps
      
      -- ** Usage Example
      -- $examplelist
      
      -- ** Tabilize methods
      Tabilize(..)

      -- ** Custom Rendering
      -- $return_box
      
    ) where

import Text.PrettyPrint.Tabilize


-- $outputexample
-- > ticker     price          marketCap
-- > "YHOO"     42.2910101         4.0e10
-- > "GOOG"     774.210101      5.3209e11
-- > "AMZN"     799.161717      3.7886e11

-- $steps
--
-- * Declare the new data type that will be the element of a list, map or a vector.
-- * The type could be a basic data constructor or a record type
-- * Derive 'Generic' and 'Data' for the type
-- * Make the type an instance of 'Text.PrettyPrint.Tabilize.Tabilize'
-- * Create values of the type and add them to a list, map or vector
-- * Use one of the 'printList', 'printMap' and 'printVector' methods to print the values in a tabular format

-- $notes
-- Note that any nested algebraic instances will be displayed by calling their respective 'Show' methods

-- $examplelist
--
-- > -- Printing a list of records
-- > :set -XDeriveGeneric
-- > :set -XDeriveDataTypeable
-- >
-- > import qualified GHC.Generics as G
-- > import Data.Data
-- >
-- > -- A record structure that will in a list
-- > data Stock = Stock {ticker::String, price::Double, marketCap::Double} deriving (Data, G.Generic)
-- >
-- > -- Create an instance of Tabilize
-- > instance Tabilize Stock
-- > 
-- > let yahoo =  Stock {ticker="YHOO", price=42.29101010, marketCap=40e9}
-- > let google = Stock {ticker="GOOG", price=774.210101, marketCap=532.09e9}
-- > let amazon = Stock {ticker="AMZN", price=799.161717, marketCap=378.86e9}
-- >
-- > -- List of records
-- > let tickers = [yahoo, google, amazon]
-- > 
-- > printList tickers
-- > 
-- > ticker     price          marketCap
-- > "YHOO"     42.2910101         4.0e10
-- > "GOOG"     774.210101      5.3209e11
-- > "AMZN"     799.161717      3.7886e11


-- $return_box
-- In cases, where the 'printList', 'printMap' or 'printVector' statements are not adequate, the 'listToBox', 'vectorToBox' and 'mapToBox' methods
-- return the B.Box value. The user can then use some of the methods provided by
-- "Text.PrettyPrint.Box" module.