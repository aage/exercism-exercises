module RnaTranscription

open System

let toRna (dna: string): string =

    let rna =
        function
        | 'G' -> 'C'
        | 'C' -> 'G'
        | 'T' -> 'A'
        | 'A' -> 'U'
        | _ -> failwith "NoNo"

    String [| for c in dna do yield rna c |]