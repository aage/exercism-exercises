module ProteinTranslation

open Microsoft.FSharp.Reflection

// taken from: https://stackoverflow.com/a/36828630/2877982
let private toString (x: 'a) =
    let (case, _) =
        FSharpValue.GetUnionFields(x, typeof<'a>)
    case.Name

let private fromString<'a> (s: string) =
    match FSharpType.GetUnionCases typeof<'a>
          |> Array.filter (fun case -> case.Name = s)
        with
    | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
    | _ -> None

type Proteine =
    | Methionine
    | Phenylalanine
    | Leucine
    | Serine
    | Tyrosine
    | Cysteine
    | Tryptophan
    override this.ToString() = toString this

type Codon =
    | AUG
    | UUU
    | UUC
    | UUA
    | UUG
    | UCU
    | UCC
    | UCA
    | UCG
    | UAU
    | UAC
    | UGU
    | UGC
    | UGG
    | UAA
    | UAG
    | UGA
    static member fromString s = fromString<Codon> s

type CodonType = | Stop | Proteine of Proteine

let private codonType =
    function
    | AUG                   -> Proteine Proteine.Methionine
    | UUU | UUC             -> Proteine Proteine.Phenylalanine
    | UUA | UUG             -> Proteine Proteine.Leucine
    | UCU | UCC | UCA | UCG -> Proteine Proteine.Serine
    | UAU | UAC             -> Proteine Proteine.Tyrosine
    | UGU | UGC             -> Proteine Proteine.Cysteine
    | UGG                   -> Proteine Proteine.Tryptophan
    | UAA | UAG | UGA       -> Stop

let proteins (rna: string) : string list =

    let rec inner acc codons =
        match codons with
        | [] -> List.rev acc
        | head :: tail ->
            match head with
            | Stop -> inner acc []
            | Proteine proteine -> inner (proteine.ToString() :: acc) tail

    rna
    |> Seq.chunkBySize 3
    |> Seq.map (System.String >> Codon.fromString)
    |> Seq.choose id
    |> Seq.map codonType
    |> List.ofSeq
    |> inner []
