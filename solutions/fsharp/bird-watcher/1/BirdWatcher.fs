module BirdWatcher

let lastWeek: int [] = [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday (counts: int []): int = counts.[counts.Length - 2]

let total (counts: int []): int = Array.sum counts

let dayWithoutBirds (counts: int []): bool = counts |> Array.contains 0

let incrementTodaysCount (counts: int []): int [] =
    let today = [| Array.last counts |> (+) 1 |]
    let tail = Array.rev counts |> Array.tail
    Array.concat (seq [| today; tail |]) |> Array.rev

let oddWeek (counts: int []): bool =

    match counts with
    | [| _; 0; _; 0; _; 0; _ |] -> true
    | [| 5; _; 5; _; 5; _; 5 |] -> true
    | [| _; 10; _; 10; _; 10; _ |] -> true
    | _ -> false
