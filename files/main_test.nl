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