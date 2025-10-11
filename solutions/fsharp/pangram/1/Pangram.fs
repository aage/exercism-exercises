module Pangram

open System

let isPangram (input: string): bool =
    let alphabet = ['a' .. 'z']
    let sanitized = input
                    |> Seq.map Char.ToLower
                    |> Seq.filter (fun c -> List.contains c alphabet)
    let length = sanitized |> Seq.distinct |> Seq.length
    let pangram = alphabet.Length = length
    pangram