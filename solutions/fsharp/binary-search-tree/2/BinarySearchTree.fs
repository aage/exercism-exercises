module BinarySearchTree

type Node =
    { Value: int
      Left: Node option
      Right: Node option }

let rec insertNode (tree: Node option) (value: int) =

    match tree with
    | None ->
        { Value = value
          Left = None
          Right = None }
    | Some t ->
        if (value <= t.Value)
        then { t with Left = Some(insertNode t.Left value) }
        else { t with Right = Some(insertNode t.Right value) }

let data (node: Node) = node.Value
let left (node: Node) = node.Left
let right (node: Node) = node.Right

let create (items: int list): Node =
    let tree = items |> List.fold (fun acc item -> Some(insertNode acc item)) None
    tree.Value

let traverse (node: Node) (f: Node -> 'a) =

    let rec inner (node: Node option) (acc: 'a list) =
        match node with
        | None -> acc
        | Some n ->
            let left = inner n.Left acc
            let right = inner n.Right acc
            [f (n)] @ left @ right
    inner (Some node) []

let sortedData (node: Node) = traverse node (fun n -> n.Value) |> List.sort