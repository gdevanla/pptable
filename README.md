# README #

Text.PrettyPrint.Tabulate : Print any list, vector or map as a well-formatted readable table.

### Text.PrettyPrint.Tabulate ###

* This module provides simple functions used to print values in tabular format
* Version 0.2.0.0
* Contributions and Bug Reports welcome. Please use the Github issue tracker.

### Example ###

``` haskell

  -- ./Example.hs
  :set -XDeriveGeneric
  :set -XDeriveDataTypeable

  import qualified GHC.Generics as G
  import Data.Data

  import qualified Text.PrettyPrint.Tabulate as T
  import qualified Data.Map as Map
  import qualified Data.List as List
  import qualified Data.Vector as Vector


  data Stock = Stock {ticker::String, price::Double, marketCap::Double} deriving (Data, G.Generic)
  instance T.Tabulate Stock

  let yahoo =  Stock {ticker="YHOO", price=42.29101010, marketCap=40e9}
  let google = Stock {ticker="GOOG", price=774.210101, marketCap=532.09e9}
  let amazon = Stock {ticker="AMZN", price=799.161717, marketCap=378.86e9}


  -- List of records
  let tickers = [yahoo, google, amazon]

  -- The record type 'Stock' can also be in a Map
  let tickers_map = Map.fromList [(10, yahoo), (100, google), (1000, amazon)]

  -- Or in a Vector
  let tickers_vector = Vector.fromList tickers

  -- Print table from List
  T.ppTable tickers
   ticker     price              marketCap
   YHOO         42.291010100     4.000000000e10
   GOOG        774.210101000     5.320900000e11
   AMZN        799.161717000     3.788600000e11

   -- Print table from Map
   T.ppTable tickers_map
   Key      ticker     price              marketCap
   10       YHOO         42.291010100     4.000000000e10
   100      GOOG        774.210101000     5.320900000e11
   1000     AMZN        799.161717000     3.788600000e11


   -- Print table from Vector
   T.ppTable tickers_vector
   ticker     price              marketCap
   YHOO         42.291010100     4.000000000e10
   GOOG        774.210101000     5.320900000e11
   AMZN        799.161717000     3.788600000e11

```
