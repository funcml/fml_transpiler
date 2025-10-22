#!/bin/sh

cabal build --with-compiler=wasm32-wasi-ghc --with-ghc-pkg=/home/jdk/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc-pkg

WASM_FILE=$(find dist-newstyle -name "fml.wasm")

if [ -f "$WASM_FILE" ]; then
  wasmtime run $WASM_FILE
else
  echo "WASM file not found!"
fi