let add1 x = x + 1

let sum x y = x + y
let result = add1 2

printfn "%A" result // 3



type Person = { name: string; age: string }

let person = { name = "John"; age = "30" }

printfn "%A" person
