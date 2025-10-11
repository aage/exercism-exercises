module RunLengthEncoding

open System
open System.Text.RegularExpressions

let encode input =

    let enc str =
        let len = String.length str
        match len with
        | 0 -> ""
        | 1 -> sprintf "%c" (Seq.head str)
        | _ -> sprintf "%i%c" len (Seq.head str)

    let breakup str =

        let mutable chunk = []
        [
            for c in str do
                if List.length chunk = 0 then
                    chunk <- [c]
                elif List.last chunk = c then
                    chunk <- chunk @ [c]
                else
                    yield chunk |> Array.ofList |> String
                    chunk <- [c]
            yield chunk |> Array.ofList |> String
        ]

    input
    |> breakup
    |> List.map enc
    |> String.concat ""

let decode input =

    let dec (str:string) =
        match String.length str with
        | 1 -> str
        | _ ->
            let rev = str |> Seq.rev |> Array.ofSeq
            let c = Array.head rev |> sprintf "%c"
            let len = Array.tail rev |> Array.rev |> String |> int
            String.replicate len c

    let rgx = Regex("((\d+\s{1})|(\d+[A-Za-z]{1})|([A-Za-z]{1})|(\s{1}))")
    
    [ for m in rgx.Matches(input) do yield dec m.Value ]
    |> String.concat "" 
