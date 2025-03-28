let bind (switchFn: 'a -> Result<'b,'e>) =
    fun (twoTrackInput: Result<'a,'e>) ->
        match twoTrackInput with
        | Ok success -> switchFn success
        | Error failure -> Error failure


