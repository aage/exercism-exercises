module Series

let slices (str:string) (length:int) : string list Option =

    match (length > String.length str, length < 1) with
    | (true, _) -> None
    | (_, true) -> None
    | _ ->
        Some [ for idx in [ 0 .. String.length str - 1 ] do
                   if Seq.length (str[idx ..]) >= length then
                       yield str[idx .. idx + length - 1] ]
