module Tournament

open System

type private Season = Team * Outcome list
and private Game = Team * Outcome
and private Team = Team of String

and private Outcome =
    | Win
    | Draw
    | Loss

let private readLine (line: String) =
    let data = line.Split(';')
    let teamL = Team data[0]
    let teamR = Team data[1]

    let outcomes =
        match data[2] with
        | "win" -> (Win, Loss)
        | "draw" -> (Draw, Draw)
        | "loss" -> (Loss, Win)
        | _ -> failwith "NoNo"

    [ Game(teamL, fst outcomes); Game(teamR, snd outcomes) ]

let private seasons (games: Game list) =
    games
    |> List.groupBy fst
    |> List.map (fun (team, games) -> Season(team, games |> List.map snd))

let private points =
    function
    | Win -> 3
    | Draw -> 1
    | Loss -> 0

let private sortDescending (seasons: Season list) =

    seasons
    |> List.sortBy (fst >> fun (Team t) -> t)
    |> List.sortByDescending (snd >> List.sumBy points)

let private header = "Team                           | MP |  W |  D |  L |  P"

let private print (season: Season) =

    let padL n (str: String) = str.PadLeft(n).Substring(0, n)
    let padR n (str: String) = str.PadRight(n).Substring(0, n)
    let howMany f = List.filter f >> List.length

    let outcomes = snd season

    let (Team t) = fst season
    let tt = padR 31 t

    let mp = List.length outcomes
    let w = howMany ((=) Win) outcomes
    let d = howMany ((=) Draw) outcomes
    let l = howMany ((=) Loss) outcomes
    let p = outcomes |> List.sumBy points |> string |> padL 3
    sprintf "%s|  %i |  %i |  %d |  %i |%s" tt mp w d l p


let tally input =

    let data =
        input |> List.collect readLine |> seasons |> sortDescending |> List.map print

    [ yield header
      for x in data do
          yield x ]
