module OcrNumbers

let breakupIntoDigits charLength charHeight rows =

    let digits (rows: string list) =
        let digitLength = rows.[0] |> String.length
        [ 0 .. charLength .. digitLength - 1 ]
        |> List.map (fun idx -> rows |> List.map (fun r -> r.[idx..idx + charLength - 1]))

    let multilineIntoSingleline (rows: string list) =
        let comma = [ "   "; "   "; "   "; "  |" ]
        [ 0 .. charLength ]
        |> List.map (fun idx ->
            let indices = [ idx .. charHeight .. (List.length rows - 1) ]
            indices
            |> List.map (fun idx' ->
                let useComma = idx' <> List.last indices
                if useComma then rows.[idx'] + comma.[idx] else rows.[idx'])
            |> String.concat "")

    let correctRowsCount = (List.length rows) % charHeight = 0
    let correctCharsLength = (String.concat "" rows |> String.length) % charLength = 0
    let useCommas = (List.length rows) > charHeight

    match (correctRowsCount, correctCharsLength, useCommas) with
    | (true, true, false) -> digits rows |> Some
    | (true, true, true) ->
        multilineIntoSingleline rows
        |> digits
        |> Some
    | _ -> None

let read digit =

    match digit with
    | [ " _ "; "| |"; "|_|"; "   " ] -> "0"
    | [ "   "; "  |"; "  |"; "   " ] -> "1"
    | [ " _ "; " _|"; "|_ "; "   " ] -> "2"
    | [ " _ "; " _|"; " _|"; "   " ] -> "3"
    | [ "   "; "|_|"; "  |"; "   " ] -> "4"
    | [ " _ "; "|_ "; " _|"; "   " ] -> "5"
    | [ " _ "; "|_ "; "|_|"; "   " ] -> "6"
    | [ " _ "; "  |"; "  |"; "   " ] -> "7"
    | [ " _ "; "|_|"; "|_|"; "   " ] -> "8"
    | [ " _ "; "|_|"; " _|"; "   " ] -> "9"
    | [ "   "; "   "; "   "; "  |" ] -> ","
    | _ -> "?"

let convert (rows: string list) =

    rows
    |> breakupIntoDigits 3 4
    |> Option.map (List.map read)
    |> Option.map (String.concat "")
