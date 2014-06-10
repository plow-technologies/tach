# tach-migration-routes

The migration portion of tach used to upload all data to s3

## Installation

```
cabal clean
cabal configure
cabal install
cabal build
```

## Usage

### important Routes
* /migration/receive/time-series-data ReceiveTimeSeriesR POST
  * Route where the tvData is posted
* /list/#String ListDataR GET
  * List all the data for an encoded key
* /kill KillNodeR GET
  * Stops the node (probably best to just kill)
* /query/timeseries QueryTimeSeriesR POST
  * Should be posted with:
  ```
  data TimeSeriesQuery = TimeSeriesQuery {
    tsqKey :: String
  , tsqStart :: Int
  , tsqEnd :: Int
  , tsqPeriod :: Int
  , tsqDelta :: Int
  }
  ```
* /stats/listSizes ListKeySortedR GET
  * Lists all keys with sizes and PID
* /start/archive StartArchiveR GET
  * Start gc on all data
* /info/gc CheckGCStateR GET
  * Get the gc state
* /info/count TotalCountR GET
  * Total count of the data


## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
