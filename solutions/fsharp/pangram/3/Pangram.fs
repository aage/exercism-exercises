module Pangram

let private lower (s: string) = s.ToLowerInvariant()

let isPangram (input: string) : bool =

    [ 'a' .. 'z' ]
    |> List.forall (fun c -> Seq.contains c (lower input))
