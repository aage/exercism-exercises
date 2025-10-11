module IsbnVerifier

open System

// isbn is made up of
// * 9 digits
// * a tenth digit or an 'X' which represents '10'
// * multiply the range [10 .. 1] to these numbers, sum them and then module 11
// * if the result is zero it is a valid isbn

type private Isbn = Isbn of int list

let private digits (isbn: string) =

    let data = isbn.Replace("-", "")

    let digits (data: char seq) =
        data
        |> Seq.map (Char.GetNumericValue >> int)
        |> List.ofSeq

    if String.length data <> 10 then
        Error "Invalid length"
    elif data
         |> Seq.take 9
         |> Seq.forall Char.IsDigit
         |> not then
        Error "Illegal data"
    elif data |> Seq.last |> Char.IsDigit then
        Ok(digits data)
    elif data |> Seq.last |> (=) 'X' then
        Ok((data |> Seq.take 9 |> digits) @ [ 10 ])
    else
        Error "Invalid data"

let private validateIsbn (digits: int list) =
    [ 1..10 ]
    |> List.rev
    |> List.zip digits
    |> List.map (fun (fct, dig) -> fct * dig)
    |> List.sum
    |> fun sum ->
        if sum % 11 = 0 then
            Ok(Isbn digits)
        else
            Error "Not a valid Isbn"

let isValid (isbn: string) =

    digits isbn
    |> Result.bind validateIsbn
    |> function
        | Ok _ -> true
        | Error _ -> false
