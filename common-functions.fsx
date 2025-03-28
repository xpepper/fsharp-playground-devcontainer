module Result =
    let bind switchFn =
        fun twoTrackInput->
            match twoTrackInput with
            | Ok success -> switchFn success
            | Error failure -> Error failure

    let map singleTrackFn =
        fun twoTrackInput ->
            match twoTrackInput with
            | Ok success -> Ok (singleTrackFn success)
            | Error failure -> Error failure
