# tach-typeclasses

TODO: Write description here

## Installation

TODO: Write installation instructions here

## Usage

TODO: Write usage instructions here

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Purpose

The typeclasses aim to provide a way of making the serialization and stored type only depend on a lower level sum type
instead of using a sum type at the top level.

This is done with the ```Classify a b``` data type which is almost exactly like Either but wrapped to control the serialization.
The types for Classify should start out as something like ```Classify (Classify (Classify () c) b) a``` where c is the first transform
that is applied that might work, b is the second transform that might work and a is the default transform that will have to work.

With this type the worst case json serialization will be something like:
```
{
  "Unlcassified":{
    "Unlcassified":{
      "Classified": c
    }
  }
}
```

and the best case json serialization will be:
```
{
  "Classified": a
}
```

With this method you are then able to change the data type from ```Classify (Classify (Classify () c) b) a``` to ```Classify (Classify (Classify d c) b) a``` (where d is any number of recursive ```Classify e f```) without compromising the previous serialization reads or writes.