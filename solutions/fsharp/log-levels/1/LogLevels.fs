module LogLevels

let private breakup (logLine: string) : (string * string) =
    let arr =
        logLine.Split(": ")
        |> Array.map (fun s -> s.Trim())

    let err = arr.[0].ToLower()
    let msg = arr.[1]
    (err.[1..(String.length err - 2)], msg)

let message (logLine: string) : string = breakup logLine |> snd

let logLevel (logLine: string) : string = breakup logLine |> fst

let reformat (logLine: string) : string =
    let (err, msg) = breakup logLine
    sprintf "%s (%s)" msg err
