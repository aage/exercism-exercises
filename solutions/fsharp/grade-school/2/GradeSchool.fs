module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =

    let students =
        match school.ContainsKey(grade) with
        | true -> student :: school.[grade] |> List.sort
        | false -> [ student ]

    school.Add(grade, students)

let roster (school: School): string list =

    school
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.collect snd
    |> List.ofSeq

let grade (number: int) (school: School): string list =

    match school.ContainsKey(number) with
    | true -> school.[number] |> List.sort
    | false -> []