module RomanNumerals

let roman arabicNumeral =

    let makeEyes length = new string('I', length)

    let translations =
        [ (1000, "M")
          (900, "CM")
          (500, "D")
          (400, "CD")
          (100, "C")
          (90, "XC")
          (50, "L")
          (40, "XL")
          (10, "X")
          (9, "IX")
          (6, "VI")
          (5, "V")
          (4, "IV") ]

    let state = makeEyes arabicNumeral
    let folder =
        fun (state: string) (translation: int * string) ->
            state.Replace(fst translation |> makeEyes, snd translation)

    translations |> List.fold folder state
