module LargestSeriesProduct

open System

let largestProduct (input: string) seriesLength: int option =

    let invalidInput =
        seriesLength < 0
        || seriesLength > input.Length
        || not (Seq.forall Char.IsDigit input)

    match (invalidInput, seriesLength = 0) with
    | (true, _) -> None
    | (_, true) -> Some 1
    | _ ->
        input
        |> Seq.map (Char.GetNumericValue >> int)
        |> Seq.windowed seriesLength
        |> Seq.map (Array.reduce (*))
        |> Seq.max
        |> Some