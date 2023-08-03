# playground

[![Visual Studio Code](https://img.shields.io/badge/vscode-%23007ACC?logo=visualstudiocode)](https://code.visualstudio.com/)
[![Haskell](https://img.shields.io/badge/haskell-%235D4F85?logo=haskell)](https://www.haskell.org/)

## Building

To compile the project, you'll need a recent release of the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/).
We recommend using [GHCup](https://www.haskell.org/ghcup/) for managing compiler versions. You'll also need
[stack](https://docs.haskellstack.org/en/stable/) for building. Then, you can just do:

```sh
stack build
stack exec playground-exe
```

## Development

This project is set up to work with [Visual Studio Code](https://code.visualstudio.com/).

### Dev Dependencies

- [pre-commit](https://pre-commit.com/)
- [HLint](https://github.com/ndmitchell/hlint#readme)

### Setup

Install [pre-commit](https://pre-commit.com/) hooks:

```sh
pre-commit install --hook-type commit-msg
```
