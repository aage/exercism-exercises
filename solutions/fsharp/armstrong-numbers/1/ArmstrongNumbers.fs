module ArmstrongNumbers

open System

let isArmstrongNumber (number: int) : bool =

    let nums =
        number.ToString() |> Seq.map (string >> int)

    let pow (x: int) = Math.Pow(x, Seq.length nums)

    nums |> Seq.sumBy pow |> (=) number
