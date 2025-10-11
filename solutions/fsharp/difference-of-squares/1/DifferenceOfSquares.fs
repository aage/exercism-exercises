module DifferenceOfSquares

let private square x = x * x

let squareOfSum (number: int) : int =

    [ 1 .. number ] |> List.sum |> square

let sumOfSquares (number: int) : int =

    [ 1 .. number ] |> List.map square |> List.sum

let differenceOfSquares (number: int) : int =
    squareOfSum number - sumOfSquares number
