module RotationalCipher

open System

let private alphabet = [ 'a' .. 'z' ]

let rotate shiftKey text =

    let rotateChar input =
        let opt =
            alphabet
            |> List.indexed
            |> List.filter (snd >> (=) (Char.ToLower(input)))
            |> List.tryHead
            |> Option.map fst

        match opt with
        | None -> input
        | Some index ->
            let c = alphabet.[(index + shiftKey) % alphabet.Length]
            if Char.IsUpper(input) then Char.ToUpper(c) else c

    String [| for c in text do yield rotateChar c |]
