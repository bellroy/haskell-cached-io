# cached-io: cache a single IO action

Sometimes you have an action that does something really expensive
whose results don't change that much. This is a simple library that
lets you cache the output of that expensive action for a
developer-specified length of time.

`test/test-cachedIO.hs` shows a very basic usage example.

## Developing this library

### Formatters

The formatters used in this repo are provided by `shell.nix`:

- `*.hs`: [`ormolu`](https://github.com/tweag/ormolu)
- `*.cabal`:
  [`cabal-fmt`](https://hackage.haskell.org/package/cabal-fmt)
  (`cabal-fmt --inplace aws-arn.cabal`)
- `*.nix`:
  [`nixpkgs-fmt`](https://github.com/nix-community/nixpkgs-fmt)
  (`nixpkgs-fmt *.nix`)

### Regenerate CI

This repo uses `haskell-ci`, which is provided by `shell.nix`:

```shell
haskell-ci regenerate
```
