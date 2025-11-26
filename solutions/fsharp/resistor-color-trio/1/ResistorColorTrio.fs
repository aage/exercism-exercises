module ResistorColorTrio

let private map =
    Map [ ("black", 0)
          ("brown", 1)
          ("red", 2)
          ("orange", 3)
          ("yellow", 4)
          ("green", 5)
          ("blue", 6)
          ("violet", 7)
          ("grey", 8)
          ("white", 9) ]

let private formatResistance (ohmString: string) : string =
    let ohms = float ohmString

    match ohms with
    | _ when ohms >= 1_000_000_000.0 ->
        let gigaohms = ohms / 1_000_000_000.0

        if gigaohms = float (int gigaohms) then
            sprintf "%.0f gigaohms" gigaohms
        else
            sprintf "%.10g gigaohms" gigaohms
    | _ when ohms >= 1_000_000.0 ->
        let megaohms = ohms / 1_000_000.0

        if megaohms = float (int megaohms) then
            sprintf "%.0f megaohms" megaohms
        else
            sprintf "%.10g megaohms" megaohms
    | _ when ohms >= 1_000.0 ->
        let kilohms = ohms / 1_000.0

        if kilohms = float (int kilohms) then
            sprintf "%.0f kiloohms" kilohms
        else
            sprintf "%.10g kiloohms" kilohms
    | _ ->
        if ohms = float (int ohms) then
            sprintf "%.0f ohms" ohms
        else
            sprintf "%.10g ohms" ohms

let label (colors: string list) =

    let num idx = map[colors[idx]]

    let one = num 0
    let two = num 1
    let three = new string ('0', (num 2))

    let str = sprintf "%i%i%s" one two three |> formatResistance

    str
