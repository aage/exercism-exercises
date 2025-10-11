module WordCount

open System
open System.Text.RegularExpressions

let countWords (phrase: string) =

    let clean s =
        Regex
            .Replace(s, "[\n:!&@$%\^\.]+", "")
            .ToLower()
            .TrimEnd(''')
            .TrimStart(''')

    phrase.Split([| ' '; ',' |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map clean
    |> Seq.filter ((=) "" >> not)
    |> Seq.groupBy id
    |> Seq.map (fun group -> (fst group, snd group |> Seq.length))
    |> Map.ofSeq
