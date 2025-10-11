module PascalsTriangle

let rows numberOfRows: int list list =

    let makeNext (prev: int list) _ =

        let surroundWithZeroes l = 0 :: l @ [ 0 ]

        prev
        |> surroundWithZeroes
        |> List.pairwise
        |> List.map (fun (a, b) -> a + b)

    match numberOfRows with
    | 0 -> []
    | _ -> [ 1 .. numberOfRows - 1 ] |> List.scan makeNext [ 1 ]