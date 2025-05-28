namespace CommonFunctions

module Result =
    let bind switchFn =
        fun twoTrackInput ->
            match twoTrackInput with
            | Ok success -> switchFn success
            | Error failure -> Error failure

    let map singleTrackFn =
        fun twoTrackInput ->
            match twoTrackInput with
            | Ok success -> Ok(singleTrackFn success)
            | Error failure -> Error failure

    let mapError liftUpFn aResult =
        match aResult with
        | Ok success -> Ok success
        | Error failure -> Error(liftUpFn failure)

    let prepend (firstResult: Result<'a, 'e>) (restOfResults: Result<'a list, 'e>) : Result<'a list, 'e> =
        match (firstResult, restOfResults) with
        | Ok first, Ok rest -> Ok(first :: rest)
        | Error first, _ -> Error first
        | _, Error rest -> Error rest

    let sequence (aListOfResults: Result<'a,'e> list): Result<'a list, 'e> =
        let initialValue = Ok []
        List.foldBack prepend aListOfResults initialValue


[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder() =
        member this.Return(x) = Ok x
        member this.Bind(x, f) = Result.bind f x

    let result = new ResultBuilder()

module Common =
    let tee (f: 'a -> unit) x =
        f x
        x

    let also (f: 'a -> unit) = Result.map (tee f)


module Examples =
    open Common

    type AError = AError of string
    type BError = BError of string
    type CError = CError of string

    type FunctionA = int -> Result<bool, AError>
    type FunctionB = bool -> Result<string, BError>
    type FunctionC = string -> Result<char list, CError>

    let functionA: FunctionA =
        fun x ->
            if x > 0 then
                Ok(x % 2 = 0)
            else
                Error(AError "Input must be greater than 0.")

    let functionB: FunctionB =
        fun x ->
            if x then
                Ok "Input is true."
            else
                Error(BError "Input is false.")

    let functionC: FunctionC =
        fun x ->
            if x.Length > 0 then
                Ok(List.ofSeq x)
            else
                Error(CError "Input string is empty.")

    type CommonErrorType =
        | AErrorCase of AError
        | BErrorCase of BError
        | CErrorCase of CError

    let functionAWithCommonError input =
        functionA input |> Result.mapError (fun (aError) -> AErrorCase aError)

    let functionBWithCommonError input =
        functionB input |> Result.mapError (fun (bError) -> BErrorCase bError)

    let functionCWithCommonError input =
        functionC input |> Result.mapError (fun (cError) -> CErrorCase cError)

    let composed input =
        input
        |> functionAWithCommonError
        |> Result.bind functionBWithCommonError
        |> Result.bind functionCWithCommonError

    let processValue (n: int) =
        Ok n
        |> also (printfn "Processing value: %d")
        |> fun v ->
            match v with
            | Ok value when value > 0 -> Ok(value * 2)
            | Ok _ -> Error "Value must be greater than 0."
            | Error _ -> Error "An error occurred."

    processValue 33

    let processValueWithComputationExpression (n: int) =
        let resultBuilder = ResultComputationExpression.ResultBuilder()

        resultBuilder {
            let! value = Ok n

            return!
                if value > 0 then
                    Ok(value * 2)
                else
                    Error "Value must be greater than 0."
        }
