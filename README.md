# newlang

A small statically typed language for Research Project: 

> Design and Implementation of a Rust-Based Compiler with Type Checking and Evaluation Mechanisms


# install program:
You need cargo, available here [rust-lang](https://www.rust-lang.org/tools/install).


Install directly from github:
``` bash
cargo install --git https:github.com/structwafel/public-newlang
```

OR:
- Clone repo
- Build program:
``` bash
cargo build --release
```
This build the program to `./target/release/newlang`
# run a newlang program
Create a script file, with the extension `.nl`


and run with the first argument the filename:
``` bash
./target/release/newlang <my-script-file.nl>

# or if moved / installed with cargo
./newlang <my-script-file.nl>
```

You can also build and run directly:
``` bash
cargo run -- <my-script-file.nl>
```

