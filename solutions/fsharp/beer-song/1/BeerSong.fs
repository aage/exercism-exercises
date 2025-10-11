module BeerSong

let private intersperse sep ls =
    List.foldBack (fun x ->
        function
        | [] -> [ x ]
        | xs -> x :: sep :: xs) ls []

let private verse startBottles =

    match startBottles with
    | 0 ->
        [ "No more bottles of beer on the wall, no more bottles of beer."
          "Go to the store and buy some more, 99 bottles of beer on the wall." ]
    | 1 ->
        [ "1 bottle of beer on the wall, 1 bottle of beer."
          "Take it down and pass it around, no more bottles of beer on the wall." ]
    | 2 ->
        [ "2 bottles of beer on the wall, 2 bottles of beer."
          "Take one down and pass it around, 1 bottle of beer on the wall." ]
    | x ->
        [ sprintf "%i bottles of beer on the wall, %i bottles of beer." startBottles startBottles
          sprintf "Take one down and pass it around, %i bottles of beer on the wall." (startBottles - 1) ]

let recite (startBottles: int) (takeDown: int) =
    let lhs = startBottles - takeDown + 1
    [ lhs .. startBottles ]
    |> List.rev
    |> List.map verse
    |> intersperse [ "" ]
    |> List.collect id
