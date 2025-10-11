module Zipper

type Tree =
    { Value: int
      Left: Tree option
      Right: Tree option }

type Zipper = { Focus: Tree; Tree: Tree }

let fromTree (tree: Tree) : Zipper = { Focus = tree; Tree = tree }

let tree (value: int) (left: Tree option) (right: Tree option) =
    { Value = value
      Left = left
      Right = right }

let toTree (zipper: Zipper) : Tree = zipper.Tree

let private traverse (replace: Tree -> Tree option) (zipper: Zipper) : Zipper =

    let rec inner (opt: Tree option) : Tree option =
        match opt with
        | None -> None
        | Some tree ->

            if tree.Value = zipper.Focus.Value
            then replace (tree)
            else
                Some
                    { Value = tree.Value
                      Left = inner tree.Left
                      Right = inner tree.Right }

    Some zipper.Tree |> inner |> Option.get |> fromTree

let value (zipper: Zipper) : int = zipper.Focus.Value

let left (zipper: Zipper) : Zipper option =

    zipper.Focus.Left
    |> Option.map (fun x -> { Focus = x; Tree = zipper.Tree })

let right (zipper: Zipper) : Zipper option =
    zipper.Focus.Right
    |> Option.map (fun x -> { Focus = x; Tree = zipper.Tree })

let up (zipper: Zipper) : Zipper option =

    let focus = zipper.Focus.Value

    let rec inner (data: (Tree * Tree option) list) : Zipper option =
        match data with
        | [] -> None
        | hd :: tl ->
            let (node, up) = hd

            if node.Value = focus
            then up |> Option.map (fun x -> { Focus = x; Tree = zipper.Tree })
            else
                let children =
                    [ node.Left; node.Right ]
                    |> List.choose id
                    |> List.map (fun x -> (x, Some node))

                List.append tl children |> inner

    inner [ zipper.Tree, None ]

let setValue (value: int) (zipper: Zipper) : Zipper =

    zipper
    |> traverse (fun tree ->
        Some
            { Value = value
              Left = tree.Left
              Right = tree.Right })

let setLeft (left: Tree option) (zipper: Zipper) : Zipper =

    zipper
    |> traverse (fun tree ->
        Some
            { Value = tree.Value
              Left = left
              Right = tree.Right })

let setRight (right: Tree option) (zipper: Zipper) : Zipper =

    zipper
    |> traverse (fun tree ->
        Some
            { Value = tree.Value
              Left = tree.Left
              Right = right })
