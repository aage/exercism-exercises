module SaddlePoints

type Coordinate = int * int
type Point = int * Coordinate

let private points (matrix: int list list) : Point list =
    [ for y in [ 0 .. List.length matrix - 1 ] do
          let row = matrix.[y]

          for x in [ 0 .. List.length row - 1 ] do
              (row[x], (y + 1, x + 1)) ]

let private saddlePoint (points: Point list) (p: Point) : bool =

    // It's called a "saddle point" because it is
    // greater than or equal to every element in its row and
    // less than or equal to every element in its column.

    let (value, (y, x)) = p

    let data = points |> List.filter ((<>) p)

    let greaterOrEqualThanRow =
        data
        |> List.filter (snd >> fst >> (=) y)
        |> List.map fst
        |> List.forall ((>=) value)

    let lessOrEqualThanColumn =
        data
        |> List.filter (snd >> snd >> (=) x)
        |> List.map fst
        |> List.forall ((<=) value)

    greaterOrEqualThanRow && lessOrEqualThanColumn

let saddlePoints (matrix:int list list) : (int * int) list =

    let points = points matrix

    points
    |> List.filter (saddlePoint points)
    |> List.map snd
