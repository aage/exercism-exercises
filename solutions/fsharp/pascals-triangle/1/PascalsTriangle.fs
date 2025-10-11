module PascalsTriangle

let rows numberOfRows : int list list =

    let rec inner (rows:int list list) rowsLeft =
        match rowsLeft with
        | 0 -> List.rev rows
        | _ ->
            let lastIndex =
                rows
                |> List.tryHead
                |> Option.map List.length
                |> Option.defaultValue 0
            let row =
                [0 .. lastIndex]
                |> List.map (fun index ->
                    if (index = 0 || index = lastIndex)
                    then 1
                    else List.head rows
                         |> fun prev -> prev.[index - 1 .. index]
                         |> List.reduce (+)
                )
            inner (row::rows) (rowsLeft - 1)

    inner [] numberOfRows
