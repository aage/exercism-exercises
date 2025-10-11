module ScrabbleScore

open System

let private scores =
    dict
        [ 1, [ 'A'; 'E'; 'I'; 'O'; 'U'; 'L'; 'N'; 'R'; 'S'; 'T' ]
          2, [ 'D'; 'G' ]
          3, [ 'B'; 'C'; 'M'; 'P' ]
          4, [ 'F'; 'H'; 'V'; 'W'; 'Y' ]
          5, [ 'K' ]
          8, [ 'J'; 'X' ]
          10, [ 'Q'; 'Z' ] ]

let private flip (xs: Collections.Generic.IDictionary<'a, 'b>) =

    xs
    |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun v -> (v, kvp.Key)))
    |> dict


let score (word: string) =

    word |> Seq.sumBy (fun c -> (flip scores).Item(Char.ToUpper c))
