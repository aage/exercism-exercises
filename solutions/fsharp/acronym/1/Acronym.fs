module Acronym

open System

let abbreviate (phrase: string) =

    phrase.Split(' ', '-')
    |> Array.filter (fun x -> x <> "")
    |> Array.map (fun x -> x.Replace("_", "").[0] |> Char.ToUpperInvariant)
    |> fun xs -> new string(xs)
