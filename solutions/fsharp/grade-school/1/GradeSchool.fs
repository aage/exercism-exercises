module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =

    let students =
        if school.ContainsKey(grade)
        then student :: school.[grade] |> List.sort
        else [ student ]

    school.Add(grade, students)

let roster (school: School): string list =

    school
    |> Map.toList
    |> List.sortBy fst
    |> List.collect snd

let grade (number: int) (school: School): string list =
    if school.ContainsKey(number)
    then school.[number] |> List.sort
    else []