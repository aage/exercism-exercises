module HighScores

let scores (values: int list): int list = values

let latest (values: int list): int =
    values
    |> List.rev
    |> List.head

let personalBest (values: int list): int =
    values
    |> List.sortDescending
    |> List.head

let personalTopThree (values: int list): int list =
    values
    |> List.sortDescending
    |> List.truncate 3
