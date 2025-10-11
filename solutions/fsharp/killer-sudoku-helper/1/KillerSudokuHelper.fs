module KillerSudokuHelper

// A 'backtracking' or 'pick or not pick' algorithm,
// based on: https://medium.com/@chaudharyritesh947/backtracking-find-all-unique-subsets-of-a-given-set-3146cd65e779
let rec private picks sets nums =
    match nums with
    | [] -> sets
    | hd :: tl ->
        picks
            [ for set in sets do
                  yield set
                  yield set @ [ hd ] ]
            tl

let combinations exclude size sum =

    [ for set in picks [ [] ] ([ 1 .. 9 ] |> List.except exclude) do
          if List.length set = size && List.sum set = sum
          then yield set ]
    |> List.sort
