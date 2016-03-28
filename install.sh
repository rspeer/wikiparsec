#!/bin/bash
cabal install hsenv
if [[ ! -d .hsenv ]]; then
    ~/.cabal/bin/hsenv || exit 1
fi
source .hsenv/bin/activate || exit 1
cabal install && ./test.sh
