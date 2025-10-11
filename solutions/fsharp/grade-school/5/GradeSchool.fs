module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =

    match school.TryFind grade with
    | Some students -> school.Add(grade, (student :: students |> List.sort))
    | None -> school.Add(grade, [ student ])

let roster (school: School): string list =

    school
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.collect snd
    |> List.ofSeq

let grade (number: int) (school: School): string list =

    Map.tryFind number school
    |> Option.defaultValue []
