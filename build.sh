#!/bin/sh
cabal build &&
cp dist-newstyle/build/x86_64-linux/ghc-9.2.4/dream-0.1.0.0/x/dreamc/build/dreamc/dreamc dreamc
cp dist-newstyle/build/x86_64-linux/ghc-9.2.4/dream-0.1.0.0/x/dreamci/build/dreamci/dreamci dreamci