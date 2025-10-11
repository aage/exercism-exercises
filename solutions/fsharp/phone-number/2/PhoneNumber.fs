module PhoneNumber

open System
open System.Text.RegularExpressions

let (>=>) result f =
    match result with
    | Ok v -> f v
    | Error e -> Error e

let validPunctuations = "(). +-"

let containsLetters input =
    match Seq.exists Char.IsLetter input with
    | true -> Error "letters not permitted"
    | false -> Ok input

let filterValidPunctuations input =
    input
    |> String.filter (fun c -> Seq.contains c validPunctuations |> not)
    |> Ok

let keepDigits input =
    input
    |> String.filter Char.IsDigit
    |> Ok

let correctCountryCode input =
    match String.length input = 11 with
    | true ->
        match input.[0] = '1' with
        | true -> Ok input.[1..] // remove '1' for easier further processing
        | false -> Error "11 digits must start with 1"
    | false -> Ok input

let correctDigitsLength input =
    match String.length input with
    | 10
    | 11 -> Ok input
    | l when l > 11 -> Error "more than 11 digits"
    | _ -> Error "incorrect number of digits"

let hasPunctuation (input: string) =
    match Regex(@"[@:!]+").Match(input).Success with
    | true -> Error "punctuations not permitted"
    | false -> Ok input

let correctAreaCode (input: string) =
    match input.[0] with
    | '0' -> Error "area code cannot start with zero"
    | '1' -> Error "area code cannot start with one"
    | _ -> Ok input

let correctExchangeCode (input: string) =
    match input.[3] with
    | '0' -> Error "exchange code cannot start with zero"
    | '1' -> Error "exchange code cannot start with one"
    | _ -> Ok input

let clean (input: string) =

    Ok input
    >=> containsLetters
    >=> filterValidPunctuations
    >=> hasPunctuation
    >=> keepDigits
    >=> correctDigitsLength
    >=> correctCountryCode
    >=> correctAreaCode
    >=> correctExchangeCode
    |> Result.map uint64
