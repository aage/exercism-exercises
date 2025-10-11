module RunLengthEncoding

open System

let private concat = String.concat ""

let encode input =

    let fmt (chunk: char list) =
        List.tryHead chunk
        |> Option.map
            (fun c ->
                match List.length chunk with
                | 1 -> sprintf "%c" c
                | len -> sprintf "%i%c" len c)
        |> Option.defaultValue ""

    let enc str =

        let mutable chunk = []

        [ for c in str do
            if List.length chunk = 0 then
                chunk <- [ c ]
            elif List.head chunk = c then
                chunk <- chunk @ [ c ]
            else
                yield fmt chunk
                chunk <- [ c ]
          yield fmt chunk ]

    enc input |> concat

let decode input =

    let dec str =

        let mutable digits = []

        [ for c in str do
              match (Char.IsDigit c, List.length digits > 0) with
              | (true, _) -> digits <- digits @ [ c ]
              | (false, true) ->
                  let num = digits |> Array.ofList |> String |> int
                  digits <- []
                  yield sprintf "%c" c |> String.replicate num
              | (false, false) -> yield sprintf "%c" c ]

    dec input |> concat
