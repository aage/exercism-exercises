module AffineCipher

open System

let private alphabet = [ 'a' .. 'z' ]
let private m = List.length alphabet

let private factors (n: int) =

    let rec inner acc ns =

        match ns with
        | [] -> acc
        | head :: tail ->
            if n % head = 0
            then
                let r = n / head
                inner ([ head; r ] @ acc) tail
            else inner acc tail

    let sqrt = Math.Sqrt(n) |> int
    inner [ 1 ] [ 2 .. sqrt ]

let private coprime (a: int) (b: int) =

    let lhs = factors a |> Set.ofList
    let rhs = factors b |> Set.ofList
    lhs |> Set.intersect rhs = Set.ofList [ 1 ]

let private index c =
    List.indexed alphabet
    |> List.filter (snd >> (=) c)
    |> List.map fst
    |> List.head

let private normalize (s:string) =
    s.ToLower()
     .Replace(" ", "")
     .Replace(",", "")
     .Replace(".", "")

let decode a b (cipheredText: string) =

    let inner c =
        let i = index c
        let mmi =
            [ 1 .. m ]
            |> List.filter (fun x -> (a * x) % m = 1)
            |> List.head

        let index = (mmi * (i - b)) % m
        if index < 0 then index + m else index

    if List.length alphabet |> coprime a |> not
    then invalidArg "a" "a is not comprime with the length of the alphabet"
    else
        [| for c in normalize cipheredText do
            if List.contains c alphabet
            then yield alphabet.[inner c]
            else yield c |]
        |> String

let encode a b (plainText: string) =

    let inner c =
        let i = index c
        ((a * i) + b) % m

    if List.length alphabet |> coprime a |> not
    then invalidArg "a" "a is not comprime with the length of the alphabet"
    else
        [| for c in normalize plainText do
            if List.contains c alphabet
            then yield alphabet.[inner c]
            else yield c |]
        |> Array.chunkBySize 5
        |> Array.map String
        |> String.concat " "
