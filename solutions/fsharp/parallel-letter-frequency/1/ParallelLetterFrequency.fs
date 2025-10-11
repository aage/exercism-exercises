module ParallelLetterFrequency

open System

let private count line =

    line
    |> Seq.fold
        (fun s c ->
            s
            |> Map.change c (function
                | Some count -> Some(count + 1)
                | None -> Some 1))
        Map.empty<char, int>

let private merge (maps: Map<char, int> seq) =

    maps
    |> Seq.concat
    |> Seq.fold
        (fun s kvp ->
            s
            |> Map.change kvp.Key (function
                | Some count -> Some(count + kvp.Value)
                | None -> Some(kvp.Value)))
        Map.empty<char, int>

let private characters line =

    String [| for c in line do
                  if Char.IsLetter c then
                      yield Char.ToLower c |]

let frequency texts =

    texts
    |> Array.ofList
    |> Array.Parallel.map (characters >> count)
    |> merge
