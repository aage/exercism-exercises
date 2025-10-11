module Raindrops

open System

let convert (number: int) : string =

    let sounds =
        [ (3, "Pling")
          (5, "Plang")
          (7, "Plong") ]
        |> List.choose (fun data ->
            if number % (fst data) = 0
            then Some(snd data)
            else None)

    if List.isEmpty sounds
    then sprintf "%d" number
    else String.Concat sounds
