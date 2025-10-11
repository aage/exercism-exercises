module ErrorHandling

let handleErrorByThrowingException () = failwith "NoNo"

let handleErrorByReturningOption (input: string) =
    try
        input |> int |> Some
    with
    | _ -> None

let handleErrorByReturningResult input =
    match handleErrorByReturningOption input with
    | None -> Error "Could not convert input to integer"
    | Some number -> Ok number

let bind switchFunction twoTrackInput =
    twoTrackInput |> Result.bind switchFunction

let cleanupDisposablesWhenThrowingException resource =

    use _ = resource
    failwith "NoNo"
