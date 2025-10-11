module Clock

open System

type Clock = Minutes of int

let time (Minutes ms) = ms

let create hours minutes: Clock =

    let minutesPerDay = 24 * 60

    let rec inner ms =

        match (ms < 0, ms >= minutesPerDay) with
        | (true, _) -> inner (ms + minutesPerDay)
        | (_, true) -> inner (ms - minutesPerDay)
        | _ -> Minutes ms

    (hours * 60) + minutes |> inner

let add minutes (clock: Clock) =
    create 0 (time clock + minutes)

let subtract minutes (clock: Clock) =
    create 0 (time clock - minutes)

let display (clock: Clock) =
    let hour, minutes = Math.DivRem(time clock, 60)
    sprintf "%02i:%02i" hour minutes
