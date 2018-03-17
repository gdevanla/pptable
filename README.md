# README #

Textme.PrettyPrint.Tabulate : Print any list, vector or map of records as a well-formatted readable table. Nested record structures are printed with hierarchically arranged headers, thus clearly showing the nested structures.

### Text.PrettyPrint.Tabulate ###

* This module provides simple functions used to print values in tabular format
* Version 0.3.0.0
* Contributions and Bug Reports welcome. Please use the Github issue tracker.

### Release Notes 0.3.0.0 (*This release has breaking changes*)

* This release has some breaking changes which was required to extend
  functionality. The extended functionality improves on previous
  release by printing nested records with hierarchical column headers

* All Records types need to update their `Tabulate` instances to provide a flag, namely `ExpandWhenNested` or `DoNotExpandWhenNested`.

* `ppTable` function has been renamed to `printTable` to make the function name more descriptive.

* New function called printTableWithFlds has been added.

### Example ###

``` haskell

    {-# LANGUAGE MultiParamTypeClasses#-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE DeriveDataTypeable #-}

    import Text.PrettyPrint.Tabulate
    import qualified GHC.Generics as G
    import Data.Data

    import qualified Text.PrettyPrint.Tabulate as T

    import qualified Data.Map as Map
    import qualified Data.List as List
    import qualified Data.Vector as Vector

```

#### Instance Declaration Requirements ####

1. All records definitions should `derive` from `Data` and `Generic`
2. All field types that are record attributes should have an instance of `CellValueFormatter`. Default instances for standard types are already provided.
3. All records that need to be printed as a table need to be an instance of `Tabulate`.
4. Depending on if the nested record's fields have to be expanded, the
   Tabulate instance could either be

   `instance Tabulate Price ExpandWhenNested` or
   `instance Tabulate Price DoNotExpandWhenNested`.

``` haskell

    data FxCode = USD | EUR | JPY deriving (Show, Data, G.Generic)
    instance T.CellValueFormatter FxCode

    -- This record type will be nested inside `Stock`
    data Price = Price {price::Double, fx_code::FxCode} deriving (Data, G.Generic, Show)

    -- if we do not want the `Price` records to be expanded into their own fields
    -- then choose `T.DoNotExpandWhenNested`
    instance T.Tabulate Price T.ExpandWhenNested
    instance T.CellValueFormatter Price

    data Stock = Stock {ticker::String, local_price::Price, marketCap::Double} deriving (
        Data, G.Generic, Show)
    instance T.Tabulate Stock T.ExpandWhenNested

```
Once we have the records and required instances created, we can see how
the created records can be viewed in the tabular format.

``` haskell

    yahoo =  Stock {ticker="YHOO", local_price=Price 42.29101010 USD, marketCap=40e9}
    google = Stock {ticker="GOOG", local_price=Price 774.210101 EUR, marketCap=532.09e9}
    amazon = Stock {ticker="AMZN", local_price=Price 799.161717 JPY, marketCap=378.86e9}

    tickers = [yahoo, google, amazon]
    tickers_vector = Vector.fromList tickers
    tickers_map:: Map.Map Integer Stock
    tickers_map = Map.fromList [(10, yahoo), (100, google), (1000, amazon)]

    printExamples:: IO ()
    printExamples = do
        putStrLn "Printing records in a list\n"
        T.printTable tickers

        putStrLn "\nPrinting records in a map with the index.\nNote the `key(s)` are printed as first columns"
        T.printTable tickers_map

        putStrLn "\nPrinting records in a vector\n"
        T.printTable tickers_vector

        -- Sometimes records may have too many fields. In those case, specific fields can
        -- be chosen to be printed. Currently, support for this functionality is
        -- minimal. The 'headers` are not printed. In the future, a function that
        -- can take header labels as a list will be provided.

        putStrLn "\nPrinting specific fields. Note, currently field names are not printed"
        T.printTableWithFlds [T.DFld (price . local_price), T.DFld ticker] tickers_map

        putStrLn "\nPrint nested record in a map, individually"
        T.printTable $ fmap local_price tickers_map

```

### Print the examples ###

``` haskell

    main:: IO ()
    main = do
        printExamples
```

### Output ###

``` haskell ignore
    Printing records in a list

    ticker     local_price        local_price     marketCap
    -          price              fx_code         -
    YHOO           42.2910101     USD               4.0000000e10
    GOOG          774.2101010     EUR               5.3209000e11
    AMZN          799.1617170     JPY               3.7886000e11

    Printing records in a map with the index (Note the `key` is printed as the first column)

    -        ticker     local_price        local_price     marketCap
    -        -          price              fx_code         -
    10       YHOO           42.2910101     USD               4.0000000e10
    100      GOOG          774.2101010     EUR               5.3209000e11
    1000     AMZN          799.1617170     JPY               3.7886000e11

    Printing records in a vector

    ticker     local_price        local_price     marketCap
    -          price              fx_code         -
    YHOO           42.2910101     USD               4.0000000e10
    GOOG          774.2101010     EUR               5.3209000e11
    AMZN          799.1617170     JPY               3.7886000e11

    Printing specific fields. Note, currently field names are not printed
    10           42.2910101     YHOO
    100         774.2101010     GOOG
    1000        799.1617170     AMZN

```
