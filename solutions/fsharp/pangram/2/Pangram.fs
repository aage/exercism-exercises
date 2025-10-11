module Pangram

open System

let isPangram (input: string): bool =
    let alphabet = [ 'a' .. 'z' ] |> Set.ofList

    let inputSet =
        input
        |> Seq.map Char.ToLower
        |> Set.ofSeq
    Set.isSubset alphabet inputSet