module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =

    let rec inner (input: 'a list) (acc: 'b list)=
        match input with
        | [] -> List.rev acc
        | head::tail ->
            inner tail ((func head) :: acc)

    inner input []
