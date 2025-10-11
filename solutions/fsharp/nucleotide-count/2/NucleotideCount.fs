module NucleotideCount

let nucleotideCounts (strand: string): Option<Map<char, int>> =

    let nucleotides = [ 'A'; 'C'; 'G'; 'T' ]

    let count s nucleotide =
        s
        |> String.filter (fun c -> c = nucleotide)
        |> String.length
        |> fun len -> (nucleotide, len)

    match (strand
           |> String.filter (fun c ->
               nucleotides
               |> List.contains c
               |> not)
           |> String.length = 0) with
    | false -> None
    | true ->
        nucleotides
        |> List.map (count strand)
        |> Map.ofList
        |> Some
