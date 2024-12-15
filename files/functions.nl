fn print_hello1(){
    print("Hello, world! 1");
}

fn print_custom(s: string){
    print(s);
}

fn test(){
    print_custom("Hello, world! 2");
}


fn execute_fn(f: fn()){
   f();
}

fn execute_string_fn(f: fn(string)){
    f("Hello, world! 3");
}

fn main(){
    print_hello1();

    test();


    execute_fn(print_hello1);

    execute_string_fn(print_custom);
}