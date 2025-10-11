module RunLengthEncoding

open System
open System.Text.RegularExpressions

let encode input =

    let translate (chain: char list) =
        match List.length chain with
        | 0 -> ""
        | 1 -> sprintf "%c" chain.[0]
        | x -> sprintf "%i%c" x chain.[0]

    let rec inner (encoded:string) (rest: char list) (chain: char list) =

        match rest with
        | [] -> sprintf "%s%s" encoded (translate chain)
        | head::tail ->
            if not (List.isEmpty chain) && chain.[0] = head
            then inner encoded tail (head :: chain)
            else inner (sprintf "%s%s" encoded (translate chain)) tail [head]

    inner "" (Seq.toList input) []

let decode input =

    let translate part =
        match String.length part with
        | 1 -> part
        | x ->
            part.[0 .. x - 2]
            |> int
            |> fun len -> new string(part.[x - 1], len)

    let getParts input =
        Regex("(\d*[A-Za-z\s]{1})").Matches(input)
        |> Seq.map (fun x -> x.Value)

    getParts input
    |> Seq.map translate
    |> fun xs -> String.Join("", xs)