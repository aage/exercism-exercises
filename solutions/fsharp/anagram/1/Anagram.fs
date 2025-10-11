module Anagram

open System

let findAnagrams sources target =

    let areEquavalent (lhs: string) (rhs: string) =

        let normalize (s: string) =
            Seq.sort (s.ToLower())
            |> Array.ofSeq
            |> fun xs -> new string (xs)

        normalize lhs = normalize rhs

    let areEqual (lhs: string) (rhs: string) = lhs.ToLower() = rhs.ToLower()

    sources
    |> List.filter (areEquavalent target)
    |> List.filter (areEqual target >> not)
