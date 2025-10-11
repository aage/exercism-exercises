module Grep

type Format =
    | Line
    | LineNumber
    | File
    | FilePrefix
    | FilePrefixLineNumber

type MatchBy =
    | Full
    | Partial
    | FullInverted
    | PartialInverted

type Case =
    | CaseSensitive
    | IgnoreCase

let private lines path =
    System.IO.File.ReadAllLines(path)
    |> Array.mapi (fun idx line -> (idx + 1, line))
    |> List.ofArray

let private fmt format index file line =

    match format with
    | Line -> line
    | LineNumber -> sprintf "%i:%s" index line
    | File -> file
    | FilePrefix -> sprintf "%s:%s" file line
    | FilePrefixLineNumber -> sprintf "%s:%i:%s" file index line

let private contains matchBy case (pattern: string) (line: string) =

    let (l, p) =
        match case with
        | CaseSensitive -> (line, pattern)
        | IgnoreCase -> (line.ToLower(), pattern.ToLower())

    match matchBy with
    | Full -> l = p
    | Partial -> l.IndexOf(p) >= 0
    | FullInverted -> l = p |> not
    | PartialInverted -> l.IndexOf(p) >= 0 |> not

let grep files flagArguments (pattern: string) =

    let hasArg arg = List.contains arg flagArguments

    let case = if hasArg "-i" then IgnoreCase else CaseSensitive
    let matchBy =
        match (hasArg "-x", hasArg "-v") with
        | (true, true) -> FullInverted
        | (true, false) -> Full
        | (false, true) -> PartialInverted
        | (false, false) -> Partial

    let format =
        match (List.length files > 1, hasArg "-n", hasArg "-l") with
        | (_, _, true) -> File
        | (true, true, _) -> FilePrefixLineNumber
        | (false, true, _) -> LineNumber
        | (true, _, _) -> FilePrefix
        | _ -> Line

    let fmt = fmt format
    let contains = contains matchBy case

    let matches =
        [ for file in files do
            for data in lines file do
                if contains pattern (snd data) then yield fmt (fst data) file (snd data) ]
    List.distinct matches
