#!/bin/sh

# Build the wasm file
./build.sh > /dev/null

# Run the wasm file with the test file as input
WASM_FILE=$(find dist-newstyle -name "fml.wasm")

if [ -f "$WASM_FILE" ]; then
  wasmtime run $WASM_FILE -- test.fml > test.html
else
  echo "WASM file not found!"
fi

# Check if the output is as expected
if [ -s test.html ]; then
  echo "Test passed!"
else
  echo "Test failed!"
fi
