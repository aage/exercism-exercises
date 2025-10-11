module Darts

type Position =
    { X: double
      Y: double }

let distance lhs rhs = abs (lhs - rhs)

let radius (left: Position) (right: Position) =
    let x = pown (distance left.X right.X) 2
    let y = pown (distance left.Y right.Y) 2
    sqrt (x + y) // use pythagorean theorem: sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

let score (x: double) (y: double): int =
    let center =
        { X = 0.0
          Y = 0.0 }
    let dart =
        { X = x
          Y = y }

    let radius = radius center dart
    match radius with
    | r when r <= 1.0 -> 10
    | r when r <= 5.0 -> 5
    | r when r <= 10.0 -> 1
    | _ -> 0
