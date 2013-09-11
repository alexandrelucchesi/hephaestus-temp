# Hephaestus SPL Temporary Repository

This is a temporary repository for making improvements on Hephaestus SPL, hosted in http://gitorious.org/hephaestus.

## Installation

It's highly recommended to use sandboxes (requires 'cabal-install' >= 1.18.0) when installing, as follows:

    $ cd alexandre/feature-modeling
    $ cabal sandbox init
    $ cabal sandbox add-source trouble-deps/benchpress-0.2.2.6
    $ cabal sandbox add-source trouble-deps/funsat-0.6.2
    $ cabal install

NOTE: In order to install benchpress, one must first have installed the C lib 'gmp'. It can be done on Mac OS X as follows:

    $ brew update
    $ brew install gmp

