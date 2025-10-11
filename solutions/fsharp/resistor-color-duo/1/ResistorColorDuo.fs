module ResistorColorDuo

let private bands =
    [ ("black", "0")
      ("brown", "1")
      ("red", "2")
      ("orange", "3")
      ("yellow", "4")
      ("green", "5")
      ("blue", "6")
      ("violet", "7")
      ("grey", "8")
      ("white", "9") ]
    |> Map.ofList

let value colors =
    colors
    |> List.take 2
    |> List.choose bands.TryFind
    |> List.reduce (+)
    |> System.Int32.Parse
