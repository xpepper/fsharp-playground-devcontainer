let addx x = x + 1

addx 1
addx 2

let add x y = x + y

add 1 2
add 3 4

// create a function to print the result of the add function
let printAdd x y = printfn "%d + %d = %d" x y (add x y)

printAdd 1 2

// create a person record
type Person = { Name: string; Age: int }

let person = { Name = "John"; Age = 30 }


