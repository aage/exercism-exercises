module Luhn

open System

let valid (number: string) =

    match number.Trim().Length with
    | 1 -> false
    | _ ->

        let data =
            number.Replace(" ", "")
            |> Seq.rev
            |> Seq.mapi
                (fun idx c ->

                    if Char.IsDigit c then
                        let fct = if idx % 2 = 1 then 2 else 1
                        let num = string c |> int |> (*) fct
                        if num > 9 then Some (num - 9)
                        else Some num
                    else None)

        Seq.forall Option.isSome data
        && (Seq.choose id data |> Seq.sum) % 10 = 0
