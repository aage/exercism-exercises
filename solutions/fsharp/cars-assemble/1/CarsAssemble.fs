module CarsAssemble

let private rate =
    function
    | s when List.contains s [ 1 .. 4 ] -> 1.0
    | s when List.contains s [ 5 .. 8 ] -> 0.9
    | 9 -> 0.8
    | 10 -> 0.77
    | _ -> 0.0

let private carsPerHour = 221

let successRate (speed: int) : float = rate speed

let productionRatePerHour (speed: int) : float =
    speed * carsPerHour |> float |> (*) (rate speed)

let workingItemsPerMinute (speed: int) : int =
    (productionRatePerHour speed) / 60.0 |> int
