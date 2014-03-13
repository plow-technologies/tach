# impulse-series-types

Impulse series contains the implementation of the data types sort of sketched in
the [time series structure notes] (https://github.com/smurphy8/time-series/wiki/Time-Series-structure-notees).

The basic idea is everything is a series of impulses.  If something is periodic, it isn't periodic forever.  The whole period is stored and represented with
a characteristic value in a parent impulse series.  The hope is to develop a language of commands that can be used across the different kinds and levels of access
that time series db often need.


## Package framework.
This package 'impulse-series-types'
is created as part of the tach framework for time series management.  Which follows the PLOW [haskell project skeleton](https://github.com/plow-technologies/plow-haskell-skeleton)



## Installation

TODO: Write installation instructions here

## Usage

TODO: Write usage instructions here

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions he
