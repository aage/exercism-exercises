module TisburyTreasureHunt

let getCoordinate (line: string * string) : string = snd line

let convertCoordinate (coordinate: string) : int * char =
    (coordinate.[0] |> string |> int, coordinate.[1])

let compareRecords
    (azarasData: string * string)
    (ruisData: string * (int * char) * string)
    : bool =

    let lhs = getCoordinate azarasData |> convertCoordinate
    let (_, rhs, _) = ruisData
    lhs = rhs

let createRecord
    (azarasData: string * string)
    (ruisData: string * (int * char) * string)
    : (string * string * string * string) =

    if compareRecords azarasData ruisData
    then
        let (four, one) = azarasData
        let (two, _, three) = ruisData
        (one, two, three, four)
    else ("", "", "", "")
