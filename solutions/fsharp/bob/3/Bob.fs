module Bob

open System

let response (input: string): string =

    let input = input.TrimEnd()

    let isSilence = input = ""
    let isQuestion = input.EndsWith("?")
    let containsLetters = String.exists Char.IsLetter input
    let allCaps = input.ToUpper() = input

    match (isSilence, isQuestion, containsLetters && allCaps) with
    | (_, true, true) -> "Calm down, I know what I'm doing!"
    | (_, _, true) -> "Whoa, chill out!"
    | (_, true, _) -> "Sure."
    | (true, _, _) -> "Fine. Be that way!"
    | _ -> "Whatever."
