module RailFenceCipher

open System

let private pattern rails len =

    let rec loop r =
        seq {
            for x in [ 1 .. r ] @ ([ 2 .. r - 1 ] |> List.rev) do
                yield x

            yield! loop r
        }

    loop rails |> Seq.take len |> List.ofSeq

let encode rails message =

    let pattern = String.length message |> pattern rails

    let data =
        message
        |> Seq.mapi (fun idx c -> (pattern[idx], c))
        |> List.ofSeq

    String [| for rail in [ 1 .. rails ] do
                  for c in
                      data
                      |> List.filter (fst >> (=) rail)
                      |> List.map snd do
                      yield c |]

let decode rails message =

    let len = String.length message
    let pattern = pattern rails len

    String [|

              for idx in [ 0 .. len - 1 ] do
                  let rail = pattern[idx]

                  let skipA =
                      pattern |> List.filter ((>) rail) |> List.length

                  let skipB =
                      pattern[ 0 .. idx - 1 ]
                      |> List.filter ((=) rail)
                      |> List.length

                  yield message[ skipA + skipB ]

               |]
