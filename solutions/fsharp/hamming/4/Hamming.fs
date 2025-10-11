module Hamming

open System

let distance (strand1: string) (strand2: string) : int option =

    if strand1.Length <> strand2.Length
    then None
    else
        [ for idx in 0 .. strand1.Length - 1 do
            yield strand1[idx] <> strand2[idx] ]
        |> List.sumBy Convert.ToInt32
        |> Some
