# README #

PrettyTable : Print any list, vector or map as a well-formatted readable table.

### Text.PrettyPrint.PrettyTable ###

* This module provides simple functions used to print values in tabular format
* Version 0.1.0
* Contributions and Bug Reports Welcome. Please use the Github issue tracker.

### Examples ###

``` haskell
 -- Printing a list of records
 :set -XDeriveGeneric
 :set -XDeriveDataTypeable

 import qualified GHC.Generics as G
 import Data.Data

 -- A record structure that will in a list
 data Stock = Stock {ticker::String, price::Double, marketCap::Double} deriving (Data, G.Generic)

 -- Create an instance of Tabilize
 instance Tabilize Stock

 let yahoo =  Stock {ticker="YHOO", price=42.29101010, marketCap=40e9}
 let google = Stock {ticker="GOOG", price=774.210101, marketCap=532.09e9}
 let amazon = Stock {ticker="AMZN", price=799.161717, marketCap=378.86e9}

 -- List of records
 let tickers = [yahoo, google, amazon]

 printList tickers

 ticker     price          marketCap
 "YHOO"     42.2910101         4.0e10
 "GOOG"     774.210101      5.3209e11
 "AMZN"     799.161717      3.7886e11

```
