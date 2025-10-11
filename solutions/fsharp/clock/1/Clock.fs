module Clock

type Clock = int * int

let create hours minutes: Clock =

    let minutesPerDay = 24 * 60

    let rec inner ms =

        match (ms < 0, ms >= minutesPerDay) with
        | (true, _) -> inner (ms + minutesPerDay)
        | (_, true) -> inner (ms - minutesPerDay)
        | _ ->
            let hour = ms / 60
            let minute = ms % 60
            (hour, minute)

    (hours * 60) + minutes |> inner

let add minutes (clock: Clock) =
    create (fst clock) (snd clock + minutes)

let subtract minutes clock =
    create (fst clock) (snd clock - minutes)

let display (clock: Clock) =
    sprintf "%02i:%02i" (fst clock) (snd clock)
