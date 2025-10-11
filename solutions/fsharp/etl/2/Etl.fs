module Etl

open System

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =

    scoresWithLetters
    |> Seq.collect (fun kvp ->
        kvp.Value
        |> Seq.map Char.ToLower
        |> Seq.map (fun c -> (c, kvp.Key)))
    |> Map.ofSeq
