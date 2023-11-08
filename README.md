# fused-logger-aeson

Structured logging, source-code compatible with 'monad-logger-aeson'.

For clients of the api, changing the import lines is all that is required.


## Install needed binaries

    cabal install hspec-discover
    cabal install doctest

## Running

    cabal repl --with-ghc=doctest

    cabal test
