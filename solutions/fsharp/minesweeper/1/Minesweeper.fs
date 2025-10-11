module Minesweeper

open System

let annotate (input: string list) : string list =

  let cell x y =
    if x < 0 || y < 0
    then 0
    else
      [ let mutable y' = 0
        for line in input do
          let mutable x' = 0
          for char in line do
            yield (x', y') = (x, y) && char = '*'
            x' <- x' + 1
          y' <- y' + 1 ]
      |> List.sumBy Convert.ToInt32

  let sweep y x c =
    match c with
    | '*' -> '*'
    | _ ->
      [ for y' in [ y - 1 .. y + 1 ] do
          for x' in [ x - 1 .. x + 1 ] do
            yield cell x' y' ]
      |> List.sum
      |> function
        | 0 -> ' '
        | x -> x.ToString()[0]

  List.mapi
    (fun y line -> line |> Array.ofSeq |> Array.mapi (sweep y) |> String)
    input
