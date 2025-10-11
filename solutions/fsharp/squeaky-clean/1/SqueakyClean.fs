module SqueakyClean

open System
open System.Text.RegularExpressions
open type System.Char

let private rgx = Regex "\p{IsGreekandCoptic}"
let private str = sprintf "%c"

let transform (c: char) : string =
    match c with
    // 1. Replace any hyphens encountered with underscores
    | '-' -> "_"
    // 2. Remove all whitespace
    | ' ' -> ""
    // 3. Convert camelCase to kebab-case
    | _ when IsUpper(c) -> ToLower(c) |> sprintf "-%c"
    // 4. Omit characters that are digits
    | _ when IsDigit(c) -> ""
    // 5. Replace Greek lower case letters with question marks
    | _ when str c |> rgx.IsMatch -> "?"
    // Otherwise return char as string
    | _ -> str c

let clean (identifier: string) : string =
    let arr =
        [| for c in identifier do
               yield transform c |]

    String.Join("", arr)
