module OcrNumbers

open System

let comma = "      |||   "
let divRem (x: int) (y: int) = Math.DivRem(x, y)
let (digitLength, digitHeight) = (3, 4)

let indicesLength len letterSize =
    divRem len letterSize
    |> fun (len, rem) ->
        if rem > 0 then len + 1 else len

let breakupInDigits (input: string list) =

    let lineLength = String.length input.[0]

    let useComma xIdx yIdx =
        List.length input > digitHeight
        && yIdx + digitHeight < List.length input && xIdx + digitLength = lineLength

    let makeDigit yIdx xIdx =
        input
        |> List.skip yIdx
        |> fun lines -> List.take (Math.Min(digitHeight, List.length lines)) lines
        |> List.map (fun s -> s.[xIdx..(xIdx + digitLength - 1)])
        |> String.concat ""

    let xs = indicesLength lineLength digitLength
    let ys = indicesLength (List.length input) digitHeight

    [ for y in 0 .. ys - 1 do
        for x in 0 .. xs - 1 do
            let xIdx = x * digitLength
            let yIdx = y * digitHeight
            yield makeDigit yIdx xIdx
            if useComma xIdx yIdx then yield comma ]

let readDigit digit =

    match String.length digit with
    | 12 ->
        match digit with
        | " _ | ||_|   " -> Some "0"
        | "     |  |   " -> Some "1"
        | " _  _||_    " -> Some "2"
        | " _  _| _|   " -> Some "3"
        | "   |_|  |   " -> Some "4"
        | " _ |_  _|   " -> Some "5"
        | " _ |_ |_|   " -> Some "6"
        | " _   |  |   " -> Some "7"
        | " _ |_||_|   " -> Some "8"
        | " _ |_| _|   " -> Some "9"
        | "      |||   " -> Some ","
        | _ -> Some "?"
    | _ -> None

let convert input =

    input
    |> breakupInDigits
    |> List.map readDigit
    |> List.reduce (fun (stateOpt: string option) (digitOpt: string option) ->
        match (stateOpt, digitOpt) with
        | (Some state, Some digit) -> sprintf "%s%s" state digit |> Some
        | _ -> None)
