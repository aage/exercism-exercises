module Change

let private updateMinimalCoins coins map target =
    // for each denomination
    coins
    // where the value is not greater than the target
    |> List.filter (fun x -> x <= target)
    // check if there are coins that make up the target minus the coin
    |> List.choose (fun x ->
        match Map.tryFind (target - x) map with
        // if there are -> prepend this coin to that list
        | Some y -> Some(x :: y)
        // otherwise -> do nothing
        | None -> None)
    // sort coins by length
    |> List.sortBy List.length
    // and take the head if the list not empty
    |> List.tryHead
    // update the map if there is data
    |> function
        | Some x -> Map.add target x map
        | None -> map

let findFewestCoins coins target =
    [ 1..target ]
    |> List.fold (updateMinimalCoins coins) (Map.ofList [ (0, []) ])
    |> Map.tryFind target
