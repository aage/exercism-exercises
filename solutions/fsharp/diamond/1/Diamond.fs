module Diamond

open System

let private fmt width data =

    let whitespace len = String(' ', len)
    // a string representation of the letter
    let letter = snd data |> sprintf "%c"
    // place space in between if necessary
    let str =
        if fst data = 0
        then letter
        else letter + (fst data |> whitespace) + letter
    // if line length is equal to total width -> return letter
    let len = String.length str
    if len = width
    then str
    else
        // else pad with missing space divided by 2 (left and right)
        let pad = (width - len) / 2 |> whitespace
        pad + str + pad

let make letter =
    // the amount of spaces between letters (0, 1, 3, 5...)
    let spaces = 0 :: [ 1 .. 2 .. 49 ]
    // combine letters with amount of space based on index
    let data =
        [ 'A' .. letter ]
        |> List.mapi
            (fun idx letter ->
                let num = spaces[idx]
                (num, letter))
        |> List.rev
    // form the top, middle (single line) and bottom of the diamond
    let top = List.tail data |> List.rev
    let middle = [ List.head data ]
    let bottom = List.tail data
    // partially apply the total width (based on widest line)
    let fmt =
        data |> List.maxBy fst |> fst |> (+) 2 |> fmt
    // concatenate the data after formatting it with padding
    top @ middle  @ bottom
    |> List.map fmt
    |> String.concat "\n"
