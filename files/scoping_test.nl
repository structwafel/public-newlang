

let y = {
    let x = 1;
    x + 8
}

if y == 9 {
    print(1)
}

if y == 10{
    print("this shouldn't happen")
} else if y == 9 {
    print("this should happen")
} else {
    print("y is not 10 or 9")
}

