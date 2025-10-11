module BinarySearch

let find input value =

    let rec inner (data: (int * int) array) =

        match data with
        | [||] -> None
        | [| x |] ->
            let (idx, v) = data.[0]
            if v = value then Some idx else None
        | _ ->
            let len = Array.length data
            let middle = len / 2
            let (idx, v) = data.[middle]

            match v.CompareTo(value) with
            | 1 -> data.[0..middle - 1] |> inner
            | -1 -> data.[middle..len - 1] |> inner
            | _ -> Some idx

    input
    |> Array.mapi (fun idx v -> (idx, v))
    |> inner
