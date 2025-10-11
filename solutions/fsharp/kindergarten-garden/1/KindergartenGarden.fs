module KindergartenGarden

type Plant =
    | Radishes
    | Clover
    | Violets
    | Grass

let students =
    [
        "Alice"; "Bob"; "Charlie"; "David"
        "Eve"; "Fred"; "Ginny"; "Harriet"
        "Ileana"; "Joseph"; "Kincaid"; "Larry" ]
    |> List.mapi (fun idx stu -> (idx + 1, stu)) // account for zero based index

let translate c =
    match c with
    | 'R' -> Radishes
    | 'V' -> Violets
    | 'G' -> Grass
    | 'C' -> Clover
    | _ -> failwith "No no"

let plants (diagram: string) (student: string) =

    let indices =
        students
        |> List.filter (fun stu -> snd stu = student)
        |> List.head
        |> fun (idx, _) ->
            let double = idx * 2
            [ double - 1 .. double ]

    diagram.Split('\n')
    |> Seq.map (Seq.map translate)
    |> Seq.collect (Seq.mapi (fun idx plant -> (idx + 1, plant)))
    |> Seq.filter (fun xs -> indices |> List.contains (fst xs))
    |> Seq.map snd
    |> List.ofSeq
