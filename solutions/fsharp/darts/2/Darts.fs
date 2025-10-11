module Darts

let score (x: double) (y: double): int =
    // use pythagorean theorem: sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
    let radius = sqrt (x * x + y * y)
    match radius with
    | _ when radius <= 1.0 -> 10
    | _ when radius <= 5.0 -> 5
    | _ when radius <= 10.0 -> 1
    | _ -> 0
