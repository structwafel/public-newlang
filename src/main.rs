use newlang::{
    ast::typechecker::check_types,
    stack::{self, StackValue},
};

fn main() {
    // first argument should be the path to the script file
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <script_file>", args[0]);
        std::process::exit(1);
    }

    let script_file = &args[1];

    if !script_file.ends_with(".nl") {
        eprintln!("{} doesn't end with .nl", script_file);
        return;
    }

    let source_code = std::fs::read_to_string(script_file).unwrap();

    let retured_value = run_on_code(&source_code);

    println!("{:?}", retured_value)
}

fn run_on_code(source_code: &str) -> StackValue {
    let ast = newlang::create_ast(&source_code);
    // dbg!(&ast);

    check_types(&ast).unwrap();

    let retured_value = stack::execute_program(ast);
    retured_value.unwrap_or(StackValue::Unit)
}

#[cfg(test)]
mod tests {
    use newlang::stack::StackValue;

    use crate::run_on_code;

    #[test]
    fn test_factorial_example() {
        let source = r#"
        fn factorial(n: int) -> int {
            if n <= 1 {
                1
            } else {
                n * factorial(n - 1)
            }
        }

        fn main() -> int{
            let result = factorial(5);
            // returns 120
            result  
        }
        "#;

        let result = run_on_code(&source);
        assert_eq!(StackValue::Integer(120), result)
    }
}
