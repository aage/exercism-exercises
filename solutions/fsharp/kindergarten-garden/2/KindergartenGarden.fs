module KindergartenGarden

type Plant = | Radishes | Clover | Violets | Grass

let students =
    [ "Alice"; "Bob"; "Charlie"; "David"
      "Eve"; "Fred"; "Ginny"; "Harriet"
      "Ileana"; "Joseph"; "Kincaid"; "Larry" ]

let translate =
    function
    | 'R' -> Some(Radishes)
    | 'V' -> Some(Violets)
    | 'G' -> Some(Grass)
    | 'C' -> Some(Clover)
    | _ -> None

let plants (diagram: string) (student: string) =

    students
    |> List.indexed
    |> List.find (snd >> (=) student)
    |> fun (fst, _) ->
        let idx = (fst + 1) * 2
        diagram.Split('\n')
        |> Seq.map (fun lane -> lane.[idx - 2..idx - 1])
    |> Seq.collect (Seq.choose translate)
    |> List.ofSeq
