module Sublist

type SublistType =
    | Equal
    | Sublist
    | Superlist
    | Unequal

let private leftContainsRight l r =

    let concat xs =
        xs |> Seq.map string |> String.concat ";"

    (concat l).Contains(concat r)

let sublist xs ys =
    match (leftContainsRight xs ys, leftContainsRight ys xs) with
    | (true, true) -> Equal
    | (true, false) -> Superlist
    | (false, true) -> Sublist
    | (false, false) -> Unequal
