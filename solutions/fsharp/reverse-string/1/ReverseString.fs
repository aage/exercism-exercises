module ReverseString

let reverse (input: string): string =
    Seq.rev input
    |> Array.ofSeq
    |> fun cs -> new string(cs)