plow-haskell-skeleton
=====================

A skeleton project for haskell libraries... separation of concerns etc


## Purpose

Haskell's cabal is easy to confuse with versions and libraries... often, only a few of the features of a library are needed by any particular other library.

A shining example of this is a set of types that are being used as a message passer.  Perhaps one of the libraries they are being passed to is somewhat older.

Because of haskell's blazing development speed, you quickly find yourself with deps problems.


## Preferred method of package generation...
This is only a skeleton and in each place you will have to make your packages and tests.
The project generator that is preferred is **hi** using the h-spec template (default as of March 2014)

## Organization and description of concerns

<name>
├── backends
├── core-libs
├── core-types
├── interface-libs
├── LICENSE
├── opts-libs
└── README.md


backends --> <name>-<backend-name> : e.g. persist-mongo
core-libs --> <name>-core : e.g. onping-permissions-core
core-types --> <name>-types : e.g. onping-permissions-types

