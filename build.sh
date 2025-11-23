#!/bin/sh

cabal build

./dist-newstyle/build/x86_64-linux/ghc-9.6.7/fml-0.1.0.0/x/fml/build/fml/fml --test

echo "Building binary for Ubuntu x64..."
cp ./dist-newstyle/build/x86_64-linux/ghc-9.6.7/fml-0.1.0.0/x/fml/build/fml/fml ./bin/fml_ubuntu_x64
echo "Binary built at ./bin/fml_ubuntu_x64"
echo "Making binary executable..."
chmod +x ./bin/fml_ubuntu_x64