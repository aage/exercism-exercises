module TwelveDays

let private lyrics =
    [ ("first", "a Partridge in a Pear Tree.")
      ("second", "two Turtle Doves, ")
      ("third", "three French Hens, ")
      ("fourth", "four Calling Birds, ")
      ("fifth", "five Gold Rings, ")
      ("sixth", "six Geese-a-Laying, ")
      ("seventh", "seven Swans-a-Swimming, ")
      ("eighth", "eight Maids-a-Milking, ")
      ("ninth", "nine Ladies Dancing, ")
      ("tenth", "ten Lords-a-Leaping, ")
      ("eleventh", "eleven Pipers Piping, ")
      ("twelfth", "twelve Drummers Drumming, ") ]

let recite start stop =

    let verse index =
        lyrics.[0..index]
        |> List.rev
        |> List.mapi (fun idx lyric ->
            let (nth, present) = lyric

            match idx with
            | 0 -> sprintf "On the %s day of Christmas my true love gave to me: %s" nth present
            | x when x = index -> sprintf "and %s" present
            | _ -> present)
        |> fun vs -> [ vs |> List.reduce (+) ]

    [ start - 1 .. stop - 1 ] |> List.collect verse
