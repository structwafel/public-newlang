
// any code will be executed in the global scope.
let x = "hello";
let y = 10;

// This will print "hello10"
if y == add_one(9) {
    print("hello10")
}


// This will error, as x is not the same type as int
// if x == add_one(10) {
//    print("hello11")
// }

// functions can be created using fn keyword, typed arguments and return types are required.
fn add_one(a: int) -> int {
    a + 1
}

// higher order functions can be created using function types
fn exec_with(f: fn(int) -> int, a: int) -> int{
    f(a)
}

// we expect this to print 11
print(exec_with(add_one, 10));

// calling function before it is defined
print(factorial(5));

// recursive function
fn factorial(n: int) -> int {
    if n <= 1 {
        // without ";" an expression will return itself 
        1
    } else{
        // without ";" an expression will return itself 
        n * factorial(n - 1)
    }
}

// lambdas can be created in any scope, they can be used to create closures
fn create_adder(n: int) -> fn(int) -> int {
    |a: int| -> int {
        a + n
    }
}

let add_five = create_adder(5);
print("add_five(10)");
print(add_five(10));


// current types are:
let int_val = 10;
let float_val = 10.0;
let string_val = "hello";
let bool_val = true;
// unit value is not used yet, but it is a type.
// any function which doesn't return anything will return unit.
// discarded values, values with ";" return unit.
let unit_val = {};


// after running all the global code, main will run
// main will return a string in this example
fn main() -> string {

    let bigger = "bigger";

    if add_one(9) == 10 {
        "hello10 again"
    } else {
        "hello11"
    }
}