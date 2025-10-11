// This is the file you need to modify for your own solution.
// The unit tests will use this code, and it will be used by the benchmark tests
// for the "Mine" row of the summary table.

// Remember to not only run the unit tests for this exercise, but also the
// benchmark tests using `dotnet run -c Release`.
// Please refer to the instructions for more information about the benchmark tests.

module TreeBuilding

open TreeBuildingTypes

type Tree =
    | Branch of int * Tree list
    | Leaf of int

let recordId t =
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t =
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t =
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []

let private (|ValidData|_|) x =
    match x.ParentId, x.RecordId with
    | 0, 0 -> Some x
    | _, 0 -> None
    | pId, rId when pId = rId -> None
    | pId, rId when pId > rId -> None
    | _ -> Some x

let buildTree records =

    match List.sortBy (fun x -> x.RecordId) records with
    | [] -> failwith "Empty input"
    | hd :: tl ->

        let map =
            [ let mutable id = -1

              for r in hd :: tl do
                  match r.RecordId = id + 1, r with
                  | true, ValidData data ->
                      let children =
                          List.choose
                              (fun c ->
                                  if c.ParentId = data.RecordId
                                  then Some c.RecordId
                                  else None) tl
                      id <- id + 1
                      data.RecordId, (Leaf data.RecordId, children)
                  | _ -> failwith "NoNo" ]
            |> Map.ofList

        let rec fetch id : Tree =
            match map.Item id with
            | _, [] -> Leaf id
            | _, children -> Branch(id, List.map fetch children)

        fetch hd.RecordId
