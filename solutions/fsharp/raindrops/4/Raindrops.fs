module Raindrops

let convert (number: int) : string =

    let sounds =
        [ (3, "Pling")
          (5, "Plang")
          (7, "Plong") ]
        |> List.choose (fun data ->
            if number % (fst data) = 0
            then Some(snd data)
            else None)
        |> String.concat ""

    if sounds = ""
    then number.ToString()
    else sounds
