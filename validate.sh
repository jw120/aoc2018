#!/bin/sh

# Run all the programs and compare with known good solutions

stack build
for f in src/*.hs; do
  g="${f##src/}" # Remove src/
  h="${g%.*}" # Remove .hs
  echo "$h"
  stack exec "$h" >"output/$h.txt"
  diff "output/$h.txt" "good/$h.txt" && echo OK
done