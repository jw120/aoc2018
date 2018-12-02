#!/bin/sh

# Run all the programs and compare with known good solutions

stack build
for f in src/*; do
  g="${f##src/}" # Remove src/
  echo "$g"
  stack exec "$g" >"output/$g.txt"
  diff "output/$g.txt" "good/$g.txt" && echo OK
done