module Acronym

open System
open System.Text.RegularExpressions

let abbreviate phrase =
    Regex.Matches(phrase, @"[A-Za-z']+") 
    |> Seq.map (fun m -> Seq.head m.Value |> Char.ToUpper)
    |> String.Concat