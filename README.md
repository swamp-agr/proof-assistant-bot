# Proof Assistant Bot

## Description

## Installation

Bot could be built via following commands:

```bash
cabal update
cabal build
cabal install --overwrite-policy=always
```

### Coq

1. Install `opam >= 2.1.0`, e.g. [from here](https://ocaml.org/docs/up-and-running#installation-for-unix).

```bash
cd $HOME
opam --version
2.1.3
```

2. Install `Coq`.

```bash
cd $HOME
opam init
eval $(opam env)
opam pin add coq 8.16.1
```

### Agda

We do not need to worry about `Agda` since it is included in package dependencies. It will be installed automatically. Meantime, Agda standard library should be installed manually.

1. Get `agda-stdlib` from [Github](https://github.com/agda/agda-stdlib/releases/tag/v1.7.1).

2. Unpack archive.

```
mkdir -p $PROOF_ASSISTANT_BOT_DIR/agda
cp agda-stdlib-1.7.1.tar.gz $PROOF_ASSISTANT_BOT_DIR/agda
cd $PROOF_ASSISTANT_BOT_DIR/agda
tar -xzvf agda-stdlib-1.7.1.tar.gz
export AGDA_STDLIB_PATH=$PROOF_ASSISTANT_BOT_DIR/agda/agda-stdlib-1.7.1
```

3. Create file `$HOME/.agda/defaults` with following content:

```
standard-library
```

4. Create file `$HOME/.agda/libraries` with following content:

```
$AGDA_STDLIB_PATH/standard-library.agda-lib
```

## Usage

## Available instances

## Acknowledgements
