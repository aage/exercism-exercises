module Grains

let square (n: int): Result<uint64, string> =
    let inBounds = 1 <= n && n <= 64
    if (not inBounds) then
        Error "square must be between 1 and 64"
    else
        let bar = n - 1
        let foo = System.Math.Pow(float 2, float bar)
        let grains = Ok(uint64 foo)
        grains

let total: Result<uint64, string> =

    let okOrZero r =
        match r with
        | Ok s -> s
        | _ -> uint64 0

    let squares = [ 1 .. 64 ] |> List.map (square >> okOrZero)

    let total =
        squares
        |> List.sum
        |> Ok

    total
