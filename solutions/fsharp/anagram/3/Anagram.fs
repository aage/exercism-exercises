module Anagram

let findAnagrams sources target =

    let areAnagrams (lhs: string) (rhs: string) =

        let normalize (s: string) =
            Seq.sort (s.ToLower())
            |> Array.ofSeq
            |> fun xs -> new string (xs)

        normalize lhs = normalize rhs
        && not (lhs.ToLower() = rhs.ToLower())

    sources |> List.filter (areAnagrams target)
