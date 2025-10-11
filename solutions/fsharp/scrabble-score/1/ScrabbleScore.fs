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

let score (word: string) =
    word
    |> Seq.sumBy (fun c ->
        scores
        |> Seq.find (fun kvp -> kvp.Value |> List.contains (Char.ToUpper c))
        |> fun score -> score.Key)
