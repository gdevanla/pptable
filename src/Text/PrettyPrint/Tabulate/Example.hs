-- | Example usage of ppTable library


module Text.PrettyPrint.Tabulate.Example
  (

    -- * Setup extensions
    -- $setup_extensions
    
    -- * Required imports
    -- $import_generics_and_data

    -- * Import 'Tabulate'
    -- $import_tabulate

    -- * Use any 'Traversable' 
    -- $import_containers

    -- * Declare an instance of Tabulate
    -- $declare_record

    -- * Sample Data
    -- $create_sample_data

    -- * Printing the table
    -- $print_table

    -- * Complete Example
    -- $complete_example

    -- * Output
    -- $output
    
    -- * Extending
    -- ** Extending for any 'Traversable'
    -- $boxable_instance
    
    -- ** Customizing formatting
    -- $cell_value_formatter
    


  ) where

import Text.PrettyPrint.Tabulate

-- $setup_extensions
-- > :set -XDeriveGeneric
-- > :set -XDeriveDataTypeable

-- $import_generics_and_data
-- Generics and Data need to be imported since
-- and type that will needs to be printed in tabular format
-- needs to derive from Generic and Data
--
-- > import qualified GHC.Generics as G
-- > import Data.Data
--

-- $import_tabulate
-- Tabulate class and ppTable function are the only declarations
-- to quickly using the library for printing in tabular format
--
-- > import qualified Text.PrettyPrint.Tabulate as T

-- $import_containers
-- The data type that we will print in tabular format will have
-- to be an element of a 'Traversable' instance. The ppTable library
-- provides default instances for 'Data.Map', 'Data.Vector' and 'Data.List'.
--
-- We import these modules here to provide examples of their use
-- 
-- > import qualified Data.Map as Map
-- > import qualified Data.List as List
-- > import qualified Data.Vector as Vector
--

-- $declare_record
--
-- A record structure  that will be an element in a 'Traversable' instance.
-- The data type has to derive from 'Generic' and 'Data'. 
-- Also an default instance of 'Tabulate' is declared
--
-- > data Stock = Stock {ticker::String, price::Double, marketCap::Double} deriving (Data, G.Generic)
-- > instance T.Tabulate Stock
--

-- $create_sample_data
-- 
-- > yahoo =  Stock {ticker="YHOO", price=42.29101010, marketCap=40e9}
-- > google = Stock {ticker="GOOG", price=774.210101, marketCap=532.09e9}
-- > amazon = Stock {ticker="AMZN", price=799.161717, marketCap=378.86e9}
-- > -- List of records
-- > tickers = [yahoo, google, amazon]
-- >
-- > -- The record type 'Stock' can also be in a Map
-- > tickers_map = Map.fromList [(10, yahoo), (100, google), (1000, amazon)]
-- >
-- > -- Or in a Vector
-- > tickers_vector = Vector.fromList tickers

-- $print_table
-- 
-- >  putStrLn "\nElements in a List, with records fields as column names\n"
-- >  T.ppTable tickers
-- >
-- >  putStrLn "\nElement in a map, with Key as first column\n"
-- >  T.ppTable tickers_map
-- >
-- >  putStrLn "\nElement in a Vector\n"
-- >  T.ppTable tickers_vector
-- 
  

-- $complete_example
--
-- > -- ./Example.hs
-- > :set -XDeriveGeneric
-- > :set -XDeriveDataTypeable
-- >
-- > import qualified GHC.Generics as G
-- > import Data.Data
-- >
-- > import qualified Text.PrettyPrint.Tabulate as T
-- > import qualified Data.Map as Map
-- > import qualified Data.List as List
-- > import qualified Data.Vector as Vector
-- >
-- >
-- > data Stock = Stock {ticker::String, price::Double, marketCap::Double} deriving (Data, G.Generic)
-- > instance T.Tabulate Stock
-- >
-- > let yahoo =  Stock {ticker="YHOO", price=42.29101010, marketCap=40e9}
-- > let google = Stock {ticker="GOOG", price=774.210101, marketCap=532.09e9}
-- > let amazon = Stock {ticker="AMZN", price=799.161717, marketCap=378.86e9}
-- >
-- >
-- > -- List of records
-- > let tickers = [yahoo, google, amazon]
--
-- > -- The record type 'Stock' can also be in a Map
-- > let tickers_map = Map.fromList [(10, yahoo), (100, google), (1000, amazon)]
-- >
-- > -- Or in a Vector
-- > let tickers_vector = Vector.fromList tickers


-- $output
-- > -- Print table from List
-- > T.ppTable tickers
-- > ticker     price              marketCap
-- > YHOO         42.291010100     4.000000000e10
-- > GOOG        774.210101000     5.320900000e11
-- >
-- > -- Print table from Map
-- > T.ppTable tickers_map
-- > Key      ticker     price              marketCap
-- > 10       YHOO         42.291010100     4.000000000e10
-- > 10       GOOG        774.210101000     5.320900000e11
-- >
-- >
-- > -- Print table from Vector
-- > T.ppTable tickers_vector
-- > ticker     price              marketCap
-- > YHOO         42.291010100     4.000000000e10
-- > GOOG        774.210101000     5.320900000e11
-- >


-- $boxable_instance
-- Any 'Traversable' containers can be extended to work with ppTable by implementing
-- an instance of 'Tabulate'

-- $cell_value_formatter
-- Default instances of 'CellValueFormatter' for 'Int', 'Integer', 'String', 'Float' and 'Double' are provided.
-- To customize formatting of any types, an specific implementation of 'CellValueFormatter' can be provided.

