module LuciansLusciousLasagna

let expectedMinutesInOven = 40

let remainingMinutesInOven = (-) expectedMinutesInOven
let preparationTimeInMinutes = (*) 2

let elapsedTimeInMinutes layers minutesInOven =
    layers |> preparationTimeInMinutes |> (+) minutesInOven
