module BeerSong

let private intersperse sep ls =
    List.foldBack (fun x ->
        function
        | [] -> [ x ]
        | xs -> x :: sep :: xs) ls []

let private verse startBottles =

    [ match startBottles with
      | 0 -> "No more bottles of beer on the wall, no more bottles of beer."
      | 1 -> "1 bottle of beer on the wall, 1 bottle of beer."
      | _ -> sprintf "%i bottles of beer on the wall, %i bottles of beer." startBottles startBottles

      match startBottles with
      | 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall."
      | 1 -> "Take it down and pass it around, no more bottles of beer on the wall."
      | 2 -> "Take one down and pass it around, 1 bottle of beer on the wall."
      | _ -> sprintf "Take one down and pass it around, %i bottles of beer on the wall." (startBottles - 1) ]

let recite startBottles takeDown =
    let endBottles = startBottles - takeDown + 1
    [ endBottles .. startBottles ]
    |> List.rev
    |> List.map verse
    |> intersperse [ "" ]
    |> List.concat
