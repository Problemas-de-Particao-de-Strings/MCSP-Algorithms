# MCSP Algorithms

[![Visual Studio Code](https://img.shields.io/badge/vscode-%23007ACC?logo=visualstudiocode)](https://code.visualstudio.com/)
[![Haskell](https://img.shields.io/badge/haskell-%235D4F85?logo=haskell)](https://www.haskell.org/)
[![pre-commit](https://img.shields.io/badge/pre--commit-%23555?logo=pre-commit)](https://github.com/pre-commit/pre-commit)

## Building

To compile the project, you'll need a recent release of the
[Glasgow Haskell Compiler](https://www.haskell.org/ghc/). We recommend using
[GHCup](https://www.haskell.org/ghcup/) for managing compiler versions. You'll also need
[stack](https://docs.haskellstack.org/en/stable/) for building. Then, you can just do:

```sh
stack build --no-run-benchmarks --no-run-tests
stack --silent exec playground
```

## Development

This project is set up to work with [Visual Studio Code](https://code.visualstudio.com/).

### Dev Dependencies

- [pre-commit](https://pre-commit.com/)
- [HLint](https://github.com/ndmitchell/hlint#readme)
- [Fourmolu](https://fourmolu.github.io/)

### Setup

Install [pre-commit](https://pre-commit.com/) hooks:

```sh
pre-commit install
```

### Commit Message

Commits should follow the [Conventional Commits](https://www.conventionalcommits.org/)
specification. Types in use can be found at
[pvdlg/conventional-commit-types](https://github.com/pvdlg/conventional-commit-types).

### Running Tests

```sh
stack test --no-run-benchmarks
```

### Running Benchmarks

```sh
stack bench --no-run-tests
```

### Haddock

HTML documentation can be generated with:

```sh
stack haddock --haddock-internal MCSP-Algorithms --open
```

For exposed modules only, you can drop `--haddock-internal`.
