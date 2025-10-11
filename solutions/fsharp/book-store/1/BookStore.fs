module BookStore

let private discounts =
    [ (2, 0.95m)
      (3, 0.9m)
      (4, 0.8m)
      (5, 0.75m) ]
    |> Map.ofList

let private uniqueSets sortBy books =

    let rec inner (acc: int list list) (books: int list) =
        match books with
        | [] -> acc
        | x :: xs ->
            let contains =
                acc
                |> List.filter (fun lst -> List.contains x lst |> not)
            // check if there's a set that does not have the head
            if List.isEmpty contains
            // no such set exists -> make a new set with the head
            then
                inner ([ x ] :: acc) xs
            // there's at least one such set -> order in given order -> prepend head to this
            else
                let others =
                    acc
                    |> List.filter (fun lst -> List.contains x lst)

                let sorted = contains |> List.sortBy sortBy
                let prepended = x :: (List.head sorted)
                let tail = (List.tail sorted) @ others
                inner (prepended :: tail) xs

    inner [] books

let private checkout (sets: int list list) =

    let price = 8m

    let discounted =
        sets
        |> List.filter (fun lst -> List.length lst > 1)
        |> List.sumBy
            (fun lst ->
                let len = List.length lst
                decimal len |> (*) price |> (*) discounts.[len])

    let undiscounted =
        sets
        |> List.filter (fun lst -> List.length lst = 1)
        |> List.length
        |> decimal
        |> (*) price

    discounted + undiscounted

let total books =

    let preferSmallSets = List.length
    let preferLargeSets = List.length >> (*) -1

    [ preferSmallSets; preferLargeSets ]
    |> List.map (fun f -> uniqueSets f books |> checkout)
    |> fun lst -> System.Math.Min(lst.[0], lst.[1])
