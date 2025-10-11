module Hamming

open System

let distance (strand1: string) (strand2: string): int option = //failwith "You need to implement this function."
    let equalLength = (strand1.Length = strand2.Length)
    if (not equalLength) then None
    else
        let hamming = Seq.zip strand1 strand2
                      |> Seq.map (fun (lhs,rhs) -> lhs <> rhs)
                      |> Seq.map (Convert.ToInt32)
                      |> Seq.sum
                      |> Option.Some
        hamming