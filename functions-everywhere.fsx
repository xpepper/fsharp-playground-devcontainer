// this is a named function
let add2 x = x + 2

// another named function
let times2 x = x * 2

// this is an anonymous function / aka lambda expression
let square = (fun x -> x * x)


let r = 2 |> add2 |> times2 |> square

printfn "result is %i" r

let listOfFunctions = [add2; times2; square];
for each in listOfFunctions do
  let result = each 4
  printfn "The result of applying the function is %i" result

let evalWith5ThenAdd2 aFunction = aFunction(5) + 2
evalWith5ThenAdd2 add2

// let add1 x = x + 1
let add3 x = x + 3
let add5 x = x + 5

let adderGenerator numberToAdd =
  fun x -> x + numberToAdd

let addFour = adderGenerator 4

let result = addFour 2
printfn "result: %i" result // => 6

//---

let sayGreeting greeting name =
  printfn "%s %s" greeting name

let sayHello = sayGreeting "Hello"
let sayCiao = sayGreeting "Ciao"

sayCiao "Piero"   // "Ciao Piero"
sayHello "Roger"  // "Hello Roger"

let (|->) x f = f x

let res = 3 |-> add2 |-> addFour
printfn "%i" res // prints 9

let add2ThanSquare x =
  x |> add2 |> square

printfn "%i" (add2ThanSquare 3) // prints 25

let addOne x = x + 1
let printOption x =
  match x with
  | Some value -> printfn "Value is %i" value
  | None -> printfn "No value"

3 |> addOne |> Some |> printOption // prints "Value is 4"


let peek = Option.map (fun n -> printfn "%i" n; n)
let peekResult: Result<int,string> -> Result<int,string> = Result.map (fun n -> printfn "%i" n; n)
let parseNumber (numberAsString: string) =
    match System.Int32.TryParse numberAsString with
    | true, number -> Some number
    | _ -> None

let safeDivide x y =
    if y = 0 then Error "Cannot divide by zero"
    else Ok (x / y)

let optionToResult errMsg = function
    | Some v -> Ok v
    | None -> Error errMsg

let printResult = function
    | Ok v -> printfn "Result is %i" v
    | Error errMsg -> printfn "Error: %s" errMsg

"33"
  |> parseNumber
  |> Option.map addFour
  |> optionToResult "Invalid number"
  |> Result.bind (fun x -> safeDivide x 2)
  |> printResult // prints "Result is 18"


let add (x: int) (y: float): float = float (x) + y

add 2 3.0 // returns 5.0
3.0 |> add 2 // returns 5.0





