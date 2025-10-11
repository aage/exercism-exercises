module Bob

open System

let private silence (input: string) = input.Trim() = ""
let private question (input: string) = input.Trim().EndsWith("?")

let private yelling (input: string) =
    input.ToUpper() = input
    && input |> Seq.exists Char.IsLetter

let response (input: string) : string =

    match (silence (input), question (input), yelling (input)) with
    | true, _, _ -> "Fine. Be that way!"
    | _, true, true -> "Calm down, I know what I'm doing!"
    | _, true, _ -> "Sure."
    | _, _, true -> "Whoa, chill out!"
    | _ -> "Whatever."
