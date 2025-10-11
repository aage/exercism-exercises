module Say

let nums =
    [ 0, "zero"
      1, "one"
      2, "two"
      3, "three"
      4, "four"
      5, "five"
      6, "six"
      7, "seven"
      8, "eight"
      9, "nine"
      10, "ten"
      11, "eleven"
      12, "twelve"
      13, "thirteen"
      14, "fourteen"
      15, "fifteen"
      20, "twenty"
      30, "thirty"
      40, "forty"
      50, "fifty"
      60, "sixty"
      70, "seventy"
      80, "eighty"
      90, "ninety" ]
    |> Map.ofList

let find key = nums.[key]
let trim (str: string) = str.Trim()

let removeTrailingZero (str: string) =
    let zero = "zero"

    if str = zero then
        zero
    else
        str.Replace(zero, "")

let translate (num: int64) =

    let inner (data: string * string) =

        let (num, postfix) = data
        let skip = num = "000" && postfix <> ""

        if skip then
            ""
        else

            let hundreds = num.[0]
            let tens = num.[1..]

            let hundreds' =
                if hundreds = '0' then
                    ""
                else
                    hundreds
                    |> string
                    |> int
                    |> find
                    |> sprintf "%s hundred"

            let tens' =
                if nums.ContainsKey(int tens) then
                    tens |> int |> find
                else
                    let h = sprintf "%c0" tens.[0] |> int |> find
                    let t = tens.[1] |> string |> int |> find
                    sprintf "%s-%s" h t

            sprintf "%s %s %s" hundreds' tens' postfix |> trim

    let number = num.ToString("D12")

    let numbers =
        [ (number.[0..2], "billion")
          (number.[3..5], "million")
          (number.[6..8], "thousand")
          (number.[9..], "") ]

    numbers
    |> List.map inner
    |> List.filter ((<>) "")
    |> String.concat " "
    |> removeTrailingZero
    |> trim

let say num =

    if num < 0L || num >= 1000000000000L then
        None
    else
        translate num |> Some
