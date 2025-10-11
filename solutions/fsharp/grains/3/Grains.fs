module Grains

let square (n: int): Result<uint64, string> =
    let inBounds = 1 <= n && n <= 64
    if (not inBounds) then Error "square must be between 1 and 64"
    else Ok(pown 2UL (n - 1))

let total: Result<uint64, string> =
    match square 64 with
    | Ok n -> Ok(n * 2UL - 1UL)
    | Error _ -> Error "received an error"
