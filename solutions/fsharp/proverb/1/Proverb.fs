module Proverb

let recite (input: string list): string list =

    match input with
    | [] -> input
    | head::_ ->
        [
            for pair in Seq.pairwise input do
                let (fst: string, snd) = (fst pair, snd pair)
                yield sprintf "For want of a %s the %s was lost." fst snd
            yield sprintf "And all for the want of a %s." head
        ]