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

[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder() =
        member __.Return(x) = Ok x
        member __.Bind(x, f) = Result.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return()

        member __.Delay(f) = f
        member __.Run(f) = f ()

        member this.While(guard, body) =
            if not (guard ()) then
                this.Zero()
            else
                this.Bind(body (), fun () -> this.While(guard, body))

        member this.TryWith(body, handler) =
            try
                this.ReturnFrom(body ())
            with e ->
                handler e

        member this.TryFinally(body, compensation) =
            try
                this.ReturnFrom(body ())
            finally
                compensation ()

        member this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable

            this.TryFinally(
                body',
                fun () ->
                    match disposable with
                    | null -> ()
                    | disp -> disp.Dispose()
            )

        member this.For(sequence: seq<_>, body) =
            this.Using(
                sequence.GetEnumerator(),
                fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))
            )

        member this.Combine(a, b) = this.Bind(a, fun () -> b ())

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
