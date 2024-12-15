let x = 10;

fn addone(a: int) -> int {
    a + 1
}

print( x + addone(x) );

let b = 10;

print(b)

let add_lambda = |a: int, b: int| -> int {
    a + b
};

print(add_lambda(1, 2))