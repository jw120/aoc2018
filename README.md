# Advent of code 2018

Programming exercises from adventofcode.com

## TODO

Make 9a faster

## Notes

day03a took more than a minute with standard boxed arrays. Using unbox (but still immutable) arrays,
reduced to a few seconds.

## Tips to build

Normal commands

To compile: stack build
To compile and run: stack run day01
To run: stack exec day01
To run doctests: stack test
To run with debug info (e.g., for day04): stack run day04 -- -d

To set up

stack build intero
stack install doctest
