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

let parse =
    function
    | 'R' -> Some TurnRight
    | 'L' -> Some TurnLeft
    | 'A' -> Some Advance
    | _ -> None

let turnLeft robot =
    match robot.direction with
    | North -> West
    | West -> South
    | South -> East
    | East -> North
    |> fun dir -> { robot with direction = dir }

let turnRight robot =
    robot
    |> (turnLeft
        >> turnLeft
        >> turnLeft)

let advance robot =

    match (robot.direction, robot.position) with
    | (North, (x, y)) -> (x, y + 1)
    | (West, (x, y)) -> (x - 1, y)
    | (South, (x, y)) -> (x, y - 1)
    | (East, (x, y)) -> (x + 1, y)
    |> fun pos -> { robot with position = pos }

let step robot instruction =
    match instruction with
    | TurnRight -> turnRight robot
    | TurnLeft -> turnLeft robot
    | Advance -> advance robot

let create direction position =
    { direction = direction
      position = position }

let move instructions robot =

    instructions
    |> Seq.choose parse
    |> Seq.fold (fun robot instruction -> step robot instruction) robot
