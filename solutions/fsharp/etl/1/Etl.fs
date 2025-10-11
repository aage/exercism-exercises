module Etl

open System

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =

    scoresWithLetters
    |> Map.toList
    |> List.collect (fun (score, letters) ->
        letters |> List.map (fun letter -> (Char.ToLower letter, score)))
    |> Map.ofList
