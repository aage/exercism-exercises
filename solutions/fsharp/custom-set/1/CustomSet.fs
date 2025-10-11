module CustomSet

type Set = { Elements: int list }

let empty = { Elements = [] }

let singleton value = { Elements = [value] }

let isEmpty set = List.isEmpty set.Elements

let size set = List.length set.Elements

let fromList list = { Elements = list |> List.sortBy id |> List.distinct }

let toList set = set.Elements

let contains value set = List.contains value set.Elements

let insert value set = value :: set.Elements |> fromList

let union left right =

    left.Elements @ right.Elements
    |> fromList

let intersection left right =

    Set.intersect (Set.ofList left.Elements) (Set.ofList right.Elements)
    |> List.ofSeq
    |> fromList

let difference left right =

    List.except (Seq.ofList right.Elements) left.Elements
    |> fromList

let isSubsetOf left right =
    
    List.except (Seq.ofList right.Elements) left.Elements
    |> List.isEmpty

let isDisjointFrom left right =

    // if two sets contain no matching elements they are disjointed
    let elements = left.Elements @ right.Elements
    let set = fromList elements
    List.isEmpty elements || List.length elements = List.length set.Elements

let isEqualTo left right = left.Elements = right.Elements