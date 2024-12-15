let a = 42;

print(a)
print(a + 5);

if a == 47 {
    print("a is 47")
} else if a == 42 {
    print("a is 42");
} else{
    print("a is not 47 or 42");
}



{
    let a = 10;
    print(a);
}
print(a);

let b = 5;
let c = 10;

print(b + c);

let d = b + c;
print(d);
print({
    let c = c + 5;
    c
})

let b = add(5,4)

fn combine(a: int, b: int) -> int{
    a + b
}

print(combine(5, 10));

fn combine(a: string, b: string) -> string{
    a + b
}
print(combine("Hello, ", "world!"));


fn call_fn(f: fn(int, int) -> int, a: int, b: int){
    print(f(a, b))
}

fn call_fn_string(f: fn(string, string) -> string, a: string, b: string){
    print(f(a, b));
}



// call_fn(combine, 5, 10); // this errors because types are not correct
//call_fn_string(combine, 5, 10); // this errors because 5 and 10 are not strings

// this does not error
call_fn_string(combine, "5", "10");

// call the equal function std
print(equal(5, 5));



// match c {
//     d if window.location == "hello" => print("c is 5"),
//     10 => print("c is 10"),
//     _ => print("c is not 5 or 10")
// }

// how to do pipes
// "2" &> print;
// "2" |> print;
// 
// 
// "2" => print
// "2" >> print
// "2" [[ print
// "2" ~ print
// "2" -> print