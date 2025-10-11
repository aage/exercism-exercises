module Say

let private english =
    [ 0L, "zero"
      1L, "one"
      14L, "fourteen"
      20L, "twenty"
      22L, "twenty-two"
      100L, "one hundred"
      123L, "one hundred twenty-three"
      1000L, "one thousand"
      1234L, "one thousand two hundred thirty-four"
      1000000L, "one million"
      1002345L, "one million two thousand three hundred forty-five"
      1000000000L, "one billion"
      987654321123L,
      "nine hundred eighty-seven billion six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three" ]
    |> Map.ofList

let say num =

    if num < 0L || num >= 1000000000000L
    then None
    else english.TryFind(num)
