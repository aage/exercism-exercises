module PhoneNumber

open System
open System.Text.RegularExpressions

let digits input = input
                   |> Seq.filter Char.IsDigit
                   |> Seq.map (Char.GetNumericValue >> int)
                   |> List.ofSeq

let validate11DigitsStartsWithOne (input:string) =

    if (input.Length <> 11) then Ok input
    elif (Char.IsDigit input.[0] |> not) then Ok input
    elif(Char.GetNumericValue input.[0] |> int = 1) then Ok input
    else Error "11 digits must start with 1"

let validateLetters (input:string) =

    if (input |> Seq.exists Char.IsLetter) then Error "letters not permitted"
    else Ok input

let validateDigitsLength (input:string) =

    let digits = digits input
    if (Seq.length digits = 9) then Error "incorrect number of digits"
    elif (Seq.length digits > 11) then Error "more than 11 digits"
    else Ok input

let validatePunctuations (input:string) =

    let hasPunctuation = Regex(@"[@:!]+").Match(input).Success
    if (hasPunctuation) then Error "punctuations not permitted"
    else Ok input

let validateAreaCode input =

    let digits = digits input
    let len = digits.Length
    if (len < 10) then Ok input
    elif (len < 12) then
        let index = if (len = 10) then 0 else 1
        match digits.[index] with
        | 0 -> Error "area code cannot start with zero"
        | 1 -> Error "area code cannot start with one"
        | _ -> Ok input
    else Ok input

let validateExchangeCode input =

    let digits = digits input
    let len = digits.Length
    if (len < 10) then Ok input
    elif (len < 12) then
        let index = if (len = 10) then 3 else 4
        match digits.[index] with
        | 0 -> Error "exchange code cannot start with zero"
        | 1 -> Error "exchange code cannot start with one"
        | _ -> Ok input
    else Ok input

let clean (input:string) =

    let validated = validateDigitsLength input
                    |> Result.bind validateLetters
                    |> Result.bind validate11DigitsStartsWithOne
                    |> Result.bind validatePunctuations
                    |> Result.bind validateAreaCode
                    |> Result.bind validateExchangeCode

    let result = validated
                 |> Result.map (Seq.filter Char.IsDigit)
                 |> Result.map (Seq.map (Char.GetNumericValue >> int))
                 |> Result.map (Seq.rev >> Seq.take 10 >> Seq.rev)
                 |> Result.map (Seq.map (string))
                 |> Result.map (Seq.reduce (+))
                 |> Result.map (UInt64.Parse)
    result
    