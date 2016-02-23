#!/bin/bash

# A script to run all the tests.

# Ideally this would work with the "cabal test" machinery, but that seems
# rather unfinished. Trying to make a "cabal test" work with HUnit sends you
# off into dark undocumented corners of Cabal configuration. So let's not.

for file in tests/*.hs
do
    runhaskell $file
done
