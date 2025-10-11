module Bob

open System.Text.RegularExpressions

let responses =
    [ ("^(?=.*[A-Z])([A-Z0-9,\s\%\^\*\@\#\$\(\!)]+)[!]?$", "Whoa, chill out!")
      ("^[A-Za-z0-9\s,:\)\.\!]+\?\s*$", "Sure.")
      ("^(?=.*[A-Z])([A-Z\s']+)\?$", "Calm down, I know what I'm doing!")
      ("^\s*$", "Fine. Be that way!") ]

let response (input: string): string =

    let opt =
        responses
        |> List.tryFind (fun (pattern, _) -> Regex(pattern).Match(input).Success)
        |> Option.map snd

    opt |> Option.defaultValue "Whatever."
