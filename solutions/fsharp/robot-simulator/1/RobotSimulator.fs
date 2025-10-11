module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { direction: Direction
      position: Position }

type Instruction =
    | TurnRight
    | TurnLeft
    | Advance

let private parse =
    function
    | 'R' -> Some TurnRight
    | 'L' -> Some TurnLeft
    | 'A' -> Some Advance
    | _ -> None

let private turnLeft robot =
    let dir =
        match robot.direction with
        | North -> West
        | West -> South
        | South -> East
        | East -> North
    { robot with direction = dir }

let private turnRight robot =
    let dir =
        match robot.direction with
        | North -> East
        | West -> North
        | South -> West
        | East -> South
    { robot with direction = dir }

let private advance robot =

    let pos = robot.position
    let pos' =
        match robot.direction with
        | North -> (fst pos, snd pos + 1)
        | West -> (fst pos - 1, snd pos)
        | South -> (fst pos, snd pos - 1)
        | East -> (fst pos + 1, snd pos)
    { robot with position = pos' }

let private step robot instruction =
    match instruction with
    | TurnRight -> turnRight robot
    | TurnLeft -> turnLeft robot
    | Advance -> advance robot

let create direction position =
    { direction = direction
      position = position }

let move (instructions: string) (robot: Robot) =

    instructions
    |> Seq.choose parse
    |> Seq.fold (fun robot instruction -> step robot instruction) robot
