module SecretHandshake

open System
open System.Text.RegularExpressions

type Command =
    | Wink = 1
    | DoubleBlink = 2
    | CloseYourEyes = 4
    | Jump = 8
    | ReverseOrder = 16

let format (c: Command) =

    let lower (s: string) = s.ToLower()

    let s = c.ToString()
    if s
       |> String.filter Char.IsUpper
       |> String.length = 1
    then lower s
    else
        Regex.Replace(
            s,
            "[a-z]{1}[A-Z]{1}",
            (fun m -> sprintf "%c %c" m.Value.[0] (Char.ToLower(m.Value.[1]))))
        |> lower

let commands number =

    let hasCmd = fun c -> enum<Command> (number) &&& c = c

    let cmds =
        Enum.GetValues(typedefof<Command>) :?> Command []
        |> Array.filter hasCmd
        |> List.ofArray

    if cmds |> List.contains Command.ReverseOrder
    then
        cmds
        |> List.except [ Command.ReverseOrder ]
        |> List.rev
        |> List.map format
    else cmds |> List.map format
