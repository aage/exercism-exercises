module RobotName

open System

type Robot = { Name:string }

// taken from: https://stackoverflow.com/a/33316100
let shuffleR (r : Random) xs =
    xs
    |> Seq.sortBy (fun _ -> r.Next())
    |> Array.ofSeq
    
let mkRobot () =
    let makeName () = 
        let shuffle = shuffleR (Random())
        let alphabet = [|'A'..'Z'|]
        let numbers = [|'0'..'9'|]
        let lhs = alphabet |> shuffle |> Array.take 2
        let rhs = numbers |> shuffle |> Array.take 3
        Array.concat [| lhs ; rhs |] |> String
    { Name = makeName () }

let name robot = robot.Name

let reset robot = mkRobot ()