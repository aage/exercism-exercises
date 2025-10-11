module House

let private verses =
    let shift = 2

    [ ("malt", "ate")
      ("rat", "killed")
      ("cat", "worried")
      ("dog", "tossed")
      ("cow with the crumpled horn", "milked")
      ("maiden all forlorn", "kissed")
      ("man all tattered and torn", "married")
      ("priest all shaven and shorn", "woke")
      ("rooster that crowed in the morn", "kept")
      ("farmer sowing his corn", "belonged to")
      ("horse and the hound and the horn", "") ]
    |> List.mapi (fun idx tpl -> (idx + shift, tpl))
    |> Map.ofList

let private noun verse = verses.Item verse |> fst
let private verb verse = verses.Item verse |> snd

let recite startVerse endVerse : string list =

    let verses verse : string =

        let mutable initial = true

        [ for idx = verse downto 1 do
              match (idx, initial) with
              | (1, true) -> yield "This is the house that Jack built."
              | (1, false) -> yield "that lay in the house that Jack built."
              | (_, true) ->
                  initial <- false
                  yield sprintf "This is the %s " (noun idx)
              | (_, false) -> yield sprintf "that %s the %s " (verb idx) (noun idx) ]
        |> String.concat ""

    [ for verse in startVerse..endVerse do
          yield verses verse ]
