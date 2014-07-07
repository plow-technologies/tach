# tach-db-types

TODO: Write description here

## Installation

TODO: Write installation instructions here

## Usage

TODO: Write usage instructions here

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here


## Types

The types final classify type is taken by using all of the classifications and creating a Classify tree.

In order to add a new classification you have to change a few things.
The first thing to change is the transformAll function. It would be best to add a transform function that takes a Foldable f  of Transformed and returns the same. To make this function you will need a lens and a add a function that adds the correct path the the classified portion of the classify "tree". Then you have to add a safecopy migrate instance from the old database