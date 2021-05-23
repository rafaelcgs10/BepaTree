# No Bepa Tree

Who knows what this repo is about?

## How to setup this:

### Option 1:
Instal GHC and Cabal, and using cabal install QuickCheck and GDP from Hackage.

### Option 2:
Install nix and run `nix-shell shell.nix`

## How to setup this:
Run

``` sh
ghci BepaTree.hs
```

and call the tests with `quickCheck`.

Example:

``` haskell
quickCheck prop_is_intersection
```

