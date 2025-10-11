module Hamming

let distance (strand1: string) (strand2: string) : int option =

    match strand1.Length = strand2.Length with
    | false -> None
    | true ->
        [ for idx in 0 .. strand1.Length - 1 do
              if strand1[idx] <> strand2[idx] then
                  yield 1 ]
        |> List.sum
        |> Some
