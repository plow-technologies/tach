plow-tach-DB
=====================

Plow Tach DB consists of a loose as possible set of modules for
defining a time series database.

They are then bundled together in different 'app' configurations.

The goal is to provide a framework for time series db creation that works for
many different projects.

The project may expand at some future date to include other kinds of databases.

The project is broken into several different classes of package

# Core Types

+ [impulse-series-types] (./core-types/tach-impulse-series-types/README.md)
+ [impulse-series-classes] (./core-types/tach-impulse-series-classes/README.md)
+ [wavelet-types] (./core-types/tach-wavelet-types/README.md)

# Repository Structure

## core-types

* binary-transform: An implementation of the binary transform, as explained
  [here](https://github.com/plow-technologies/writings/tree/master/binary-transform).
* tach-db-types: ?
* tach-impulse-series-classes: Retrievable class (which doesn't make much sense?).
* tach-impulse-series-types:
  Types related with time series (`TVKey`,`TVSimple`, etc).
  Also, the record type `ImpulseSeries`.
  Depends on [directed-keys](https://github.com/plow-technologies/directed-keys).
* tach-migration-acidic: Core types for the acid framework (like `TVSimpleImpulseTypeStore`).
* tach-migration-routes: The migration portion of tach used to upload all data to s3.
* tach-migration-types: `MigrationTransport` type and `IncomingKey` synonym.
* tach-transformable-types: `WaveletTransform` type, isomorphic to `[[a]]`. Impulse and Wavelet
                            types and functions.
* tach-typeclasses: Some type classes.
  + `Insertable` abstracts the idea of data structure that
    can contain zero or more elements (not that you can extract them).
  + `Bound`. Types that can be mapped to a pair of `Int`s.
  + `HasPeriodic`. ?
  + `Queryable`. This seems to be a type class to get portions of some data in an `Insertable`
                 data structure.
* tach-wavelet-types: More types related with wavelets. Only `TachWaveletRep` (which is a list
                      of `Vector`s of `Double`s) and `TachWavlet` are defined here, along with
                      the `WavletValueSeries` type synonym, related with impulse series.

## core-libs

* tach-acid-impulse-lib: Handles the acid state for impulse series inside of tach.
* tach-acid: Different acid operations.
* tach-db-acidic: Similar to tach-acid.
* tach-periodic: Defines types that represent sequences of periodic data (`PeriodicData`),
                 aperiodic data (`APeriodicData`), or a combination of both (`TVData`).
                 Also, some functions on them.
* tach-wavelet-core: Just an empty package.

## interface-libs

Nothing.

## opts-libs

Nothing.

## backends

Nothing.

## apps

Some yesod applications:

* tach-manager-impulse
* tach-node-impulse
* tach-query
