module NucleotideCount

let nucleotideCounts strand =

    let map = Map.ofList [ ('A', 0); ('C', 0); ('G', 0); ('T', 0) ] |> Some

    Seq.fold
        (fun opt c ->
            Option.filter (Map.containsKey c) opt
            |> Option.map (fun map' ->
                let count = Map.find c map' |> (+) 1
                Map.add c count map'))
        map
        strand
