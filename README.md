# Hephaestus SPL

## Installation

It's highly recommended to use sandboxes (requires "cabal-install" >= 1.18.0) when installing, as follows:

    $ cd ${YOUR_CHECKOUT_DIR}/alexandre/feature-modeling
    $ cabal sandbox init
    $ cabal sandbox add-source trouble-deps/benchpress-0.2.2.6
    $ cabal sandbox add-source trouble-deps/funsat-0.6.2
    $ cabal install

NOTE: In order to install benchpress, one must first have installed the C lib "gmp". It can be done on Mac OS X as follows:

    $ brew update
    $ brew install gmp

*: Before running the steps above, ensure that "main-is" is set to "Main.hs" inside "feature-modeling.cabal".

## Running

If the above steps ran correctly, an executable file named "hfm" must be placed inside ".cabal-sandbox/bin".

