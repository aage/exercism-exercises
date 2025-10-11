module Bob

open System
open System.Text.RegularExpressions

let matches pattern input =

    Regex(pattern).Match(input).Success

let response (input: string): string =

    match input with
    | text when text
        |> matches "^(?=.*[A-Z])([A-Z0-9,\s\%\^\*\@\#\$\(\!)]+)[!]?$"
        -> "Whoa, chill out!"
    | text when text
        |> matches "^[A-Za-z0-9\s,:\)\.\!]+\?\s*$"
        -> "Sure."
    | text when text
        |> matches "^(?=.*[A-Z])([A-Z\s']+)\?$"
        -> "Calm down, I know what I'm doing!"
    | text when String.IsNullOrWhiteSpace(text)
        -> "Fine. Be that way!"
    | _ -> "Whatever."