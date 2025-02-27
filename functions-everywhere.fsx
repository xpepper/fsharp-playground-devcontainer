let add2 x = x + 2

let times2 x = x * 2

let square = (fun x -> x * x)

let r = 2 |> add2 |> times2 |> square

printfn "result is %i" r

