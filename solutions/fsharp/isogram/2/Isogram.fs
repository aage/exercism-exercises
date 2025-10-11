module Isogram

open System

let isIsogram (str:string) =
    
    let sanitized = str.ToLower()
                    |> Seq.filter Char.IsLetter
    sanitized |> Seq.distinct |> (=) sanitized