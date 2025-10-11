module KindergartenGarden

type Plant = | Radishes | Clover | Violets | Grass

let students =
    [ "Alice"; "Bob"; "Charlie"; "David"
      "Eve"; "Fred"; "Ginny"; "Harriet"
      "Ileana"; "Joseph"; "Kincaid"; "Larry" ]

let indexOf student =
    students
    |> List.indexed
    |> List.find (snd >> (=) student)
    |> fst

let plant =
    function
    | 'R' -> Some Radishes
    | 'V' -> Some Violets
    | 'G' -> Some Grass
    | 'C' -> Some Clover
    | _ -> None

let plants (diagram: string) (student: string) =

    diagram.Split('\n') |> List.ofArray
    |> List.collect (
        Seq.skip (indexOf student * 2)
        >> Seq.take 2
        >> List.ofSeq)
    |> List.choose plant
