module RnaTranscription

open System

let toRna (dna: string): string =

    let rna =
        function
        | 'G' -> Some 'C'
        | 'C' -> Some 'G'
        | 'T' -> Some 'A'
        | 'A' -> Some 'U'
        | _ -> None

    String [| for c in dna do yield rna c |> Option.get |]