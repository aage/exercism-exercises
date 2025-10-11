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

    let pattern = String.length message |> pattern rails

    let mutable skip = 0

    let mutable map =
        pattern
        |> List.groupBy id
        |> List.map
            (fun (key, values) ->
                let len = List.length values
                let str = message[skip .. skip + len]
                skip <- skip + len
                (key, (str, 0)))
        |> Map.ofList

    String [|

              for rail in pattern do
                  let (str, idx) = map[rail]
                  let c = str[idx]

                  map <-
                      map
                      |> Map.change rail (fun _ -> Some(str, idx + 1))

                  yield c |]
