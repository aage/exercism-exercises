module LargestSeriesProduct

open System

let log msg = IO.File.AppendAllLines(@"D:\Temp\product.log", [| msg |])

let largestProduct (input: string) span: int option =

    match (input, span) with
    | (i, s) when (i.Length < s || s < 0) -> None
    | (i, _) when not (Seq.forall Char.IsDigit i) -> None
    | (i, s) when (i.Length = 0 || s = 0) -> Some 1
    | _ ->
        let digits =
            input
            |> Seq.map (fun d -> d.ToString() |> Convert.ToInt32)
            |> List.ofSeq

        let ranges = [ 0 .. input.Length - span ] |> List.map (fun i -> (i, i + (span - 1)))
        let listsOfDigits = ranges |> List.map (fun range -> digits.[fst range..snd range])
        let products = listsOfDigits |> List.map (fun ds -> ds |> List.reduce (fun a b -> a * b))

        let largest = List.max products
        Some largest
