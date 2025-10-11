module Isogram

let isIsogram (str:string) =
    let whitelist = ['a'..'z']
    let sanitized = str.ToLower()
                    |> Seq.filter (fun x -> List.contains x whitelist)
    let unique = Seq.distinct sanitized
    Seq.length sanitized = Seq.length unique