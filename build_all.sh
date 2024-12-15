#!/bin/bash

OUTPUT_DIR="binaries"
mkdir -p $OUTPUT_DIR

rustup target add x86_64-pc-windows-gnu
rustup target add x86_64-apple-darwin
rustup target add aarch64-apple-darwin
rustup target add x86_64-unknown-linux-gnu

# windows
cross build --release --target x86_64-pc-windows-gnu
mv target/x86_64-pc-windows-gnu/release/newlang.exe $OUTPUT_DIR/newlang-windows.exe

# intel mac
cross build --release --target x86_64-apple-darwin
mv target/x86_64-apple-darwin/release/newlang $OUTPUT_DIR/newlang-macos-intel

# arm mac
cross build --release --target aarch64-apple-darwin
mv target/aarch64-apple-darwin/release/newlang $OUTPUT_DIR/newlang-macos-m1

# linux
cross build --release --target x86_64-unknown-linux-gnu
mv target/x86_64-unknown-linux-gnu/release/newlang $OUTPUT_DIR/newlang-linux

echo "completed, everything in  $OUTPUT_DIR."