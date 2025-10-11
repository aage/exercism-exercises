module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =

    let students =
        match Map.tryFind grade school with
        | Some g -> student :: g |> List.sort
        | None -> [ student ]

    school.Add(grade, students)

let roster (school: School): string list =

    school
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.collect snd
    |> List.ofSeq

let grade (number: int) (school: School): string list =

    match Map.tryFind number school with
    | Some s -> s |> List.sort
    | None -> []
