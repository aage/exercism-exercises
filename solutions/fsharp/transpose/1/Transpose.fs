module Transpose

open System

let transpose (input: string list) : string list =

    match input with
    | [] -> []
    | _ ->
        let width =
            input |> List.map String.length |> List.max

        [ for idx in [ 0 .. width - 1 ] do

              input
              |> List.map (Seq.tryItem idx)
              |> List.rev
              |> List.skipWhile Option.isNone
              |> List.map (Option.defaultValue ' ')
              |> List.rev
              |> Array.ofList
              |> String ]
