module BeerSong

open System

let private sprintfUntyped format ([<ParamArray>] arr) = String.Format(format, arr)

let private intersperse sep ls =
    List.foldBack (fun x ->
        function
        | [] -> [ x ]
        | xs -> x :: sep :: xs) ls []

let private verse bottles =

    let bits =
        [ "{0} of beer on the wall, {1} of beer."
          "Go to the store and buy some more, 99 bottles of beer on the wall."
          "Take {0} down and pass it around, {1} of beer on the wall." ]

    match bottles with
    | 0 ->
        [ sprintfUntyped bits.[0] [| "No more bottles"; "no more bottles" |]
          bits.[1] ]
    | 1 ->
        [ sprintfUntyped bits.[0] [| "1 bottle"; "1 bottle" |]
          sprintfUntyped bits.[2] [| "it"; "no more bottles" |] ]
    | 2 ->
        [ sprintfUntyped bits.[0] [| "2 bottles"; "2 bottles" |]
          sprintfUntyped bits.[2] [| "one"; "1 bottle" |] ]
    | _ ->
        [ sprintfUntyped bits.[0]
              [| sprintf "%i bottles" bottles
                 sprintf "%i bottles" bottles |]
          sprintfUntyped bits.[2]
              [| "one"
                 sprintf "%i bottles" (bottles - 1) |] ]

let recite (startBottles: int) (takeDown: int) =
    let lhs = startBottles - takeDown + 1
    [ lhs .. startBottles ]
    |> List.rev
    |> List.map verse
    |> intersperse [ "" ]
    |> List.concat
