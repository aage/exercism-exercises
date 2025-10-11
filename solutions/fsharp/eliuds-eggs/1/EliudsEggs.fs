module EliudsEggs

let private binary (n: int) = System.Convert.ToString(n, 2)

let private eggs binary =
    binary |> Seq.filter ((=) '1') |> Seq.length

let eggCount n = n |> binary |> eggs
