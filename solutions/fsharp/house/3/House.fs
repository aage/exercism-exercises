module House

open System

let recite startVerse endVerse : string list =

    let verses =
        [ "house that Jack built."
          "malt that lay in"
          "rat that ate"
          "cat that killed"
          "dog that worried"
          "cow with the crumpled horn that tossed"
          "maiden all forlorn that milked"
          "man all tattered and torn that kissed"
          "priest all shaven and shorn that married"
          "rooster that crowed in the morn that woke"
          "farmer sowing his corn that kept"
          "horse and the hound and the horn that belonged to" ]

    let verse v =
        "This is"
        :: [ for idx = v - 1 downto 0 do
                 yield " the " + verses[idx] ]

    [ startVerse..endVerse ] |> List.map (verse >> String.Concat)
