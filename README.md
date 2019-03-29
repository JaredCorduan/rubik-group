# rubik-group

An implementation of the Rubik's Cube algebra as described in
[Joyner's wonderful book](https://www.maa.org/press/maa-reviews/adventures-in-group-theory).

If you take the Rubik's Cube apart (without peeling off any stickers)

![disassembled-cube][https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Disassembled-rubix-1.jpg/320px-Disassembled-rubix-1.jpg]

and put it back together anyway that the pieces will fit,
you get a permutation of the stickers that may or may not be solveable anymore.
The collection of all such permutations is what Joyner calls the the
__illegal Rubik's Cube group__, and has been implemented as `IRubik`.
Investigating this group provides insight into the actual Rubik's Cube group,
implemented here as `Rubik`.

The Rubik's Cube group is the subgroup of the illegal group consisting of all elements satifying:

* sum of the corner permutations is zero (mod 3)
* sum of the edge permutations is zero (mod 2)
* corner permutation sign = edge permutation sign

See Theorem 11.2.1 of Joyner.

There is more documentation in the haddock.

## Building the Haddock documentation:

The documentation can be built with stack:

```shell
stack build --haddock
```

## Testing

The tests can be run with stack:

```shell
stack test --pedantic
```

While developing the models, it can be helpful to run ghcid in a separate shell:

```shell
make ghcid
```

or with tests included:

```shell
make ghcid-test
```
