module CollatzConjecture

let steps (number: int): int option =

    let rec inner acc n =
        match n with
        | 1 -> acc
        | _ ->
            if n % 2 = 0
            then inner (acc + 1) (n / 2)
            else inner (acc + 1) (n * 3 |> (+) 1)

    if number < 1 then None else inner 0 number |> Some
