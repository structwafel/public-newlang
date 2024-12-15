# newlang

A small statically typed language for Research Project: Design and Implementation of a Rust-Based Compiler with Type Checking and Evaluation Mechanisms


# How to run:
You need cargo, available here [rust-lang](https://www.rust-lang.org/tools/install).

Create a script file, with the extension `.nl`

Build program:
``` bash
cargo build --release
```
This build the program to `./target/release/newlang`

and run with the first argument the filename:
``` bash
./target/release/newlang <my-script-file.nl>

# or if moved
./newlang <my-script-file.nl>
```

You can also build and run directly:
``` bash
cargo run -- <my-script-file.nl>
```

You can also find binaries created at the release page