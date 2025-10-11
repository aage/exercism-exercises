module PigLatin

open System

type private Sound = | Consonant | Vowel

let private sound c =

    let vowels = ['a'; 'e' ; 'i' ;'o' ; 'u' ]
    let consonants = ['a' .. 'z'] |> List.except (Seq.ofList vowels)

    let containedBy letters = List.contains c letters

    match (containedBy vowels, containedBy consonants) with
    | (true, false) -> Some Vowel
    | (false, true) -> Some Consonant
    | _ -> None

let translate (input:string) : string =

    // Rule 1: starts with vowel -> add 'ay' to the end
    // Rule 2: starts with consonant sound (can be multiple) -> move to end and add 'ay'
    // Rule 3: starts with consonant followed by 'qu' -> move to end and add 'ay'
    // Rule 4: contains 'y' after consonant cluster (or as second in two letter word)

    let ``pig latin`` (word:string) : string =

        match (sound word[0], word[0..2] |> List.ofSeq) with
        | (Some Vowel, _) -> word + "ay"
        | (Some Consonant, ['s' ; 'q' ; 'u']) -> word[3..] + "squay"
        | (Some Consonant, ['q' ; 'u' ; _ ]) -> word[2..] + "quay"
        | (Some Consonant, ['y' ; 't' ; _ ]) -> word + "ay"
        | (Some Consonant, ['x' ; 'r' ; _ ]) -> word + "ay"
        | (Some Consonant, [_ ; 'y' ]) ->  "y" + sprintf "%c" word[0] + "ay"
        | (Some Consonant, [_ ; _ ; 'y']) -> word[2..] + word[0..1] + "ay"
        | (Some Consonant, _) ->
            let consonants =
                word
                |> Seq.takeWhile (sound >> (=) (Some Consonant))
                |> Array.ofSeq
                |> String
            word[Seq.length consonants ..] + consonants + "ay"
        | _ -> word
        
    input.Split(" ")
    |> Array.map ``pig latin``
    |> String.concat " "