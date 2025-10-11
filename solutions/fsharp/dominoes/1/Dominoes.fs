module Dominoes

let rec canChain input =

    let connects left right =
        fst left = fst right
        || fst left = snd right
        || snd left = fst right
        || snd left = snd right

    let connectsWithTwoOthersAtLeast domino others =
        others
        |> List.filter (fun right -> connects domino right)
        |> List.length
        >= 2

    match input with
    | [] -> true
    | [ x ] -> fst x = snd x
    | _ ->
        [ 0 .. List.length input - 1 ]
        |> List.forall (fun idx ->
            let domino = input.[idx]
            let others = input |> List.except (seq { domino })
            connectsWithTwoOthersAtLeast domino others)