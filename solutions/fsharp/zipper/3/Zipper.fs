module Zipper

type 'a Zipper = { Focus: 'a Tree; Path: 'a Path list }
and 'a Path = Direction * 'a Tree

and Direction =
    | Left
    | Right

and 'a Tree =
    { Value: 'a
      Left: 'a Tree option
      Right: 'a Tree option }

let tree a l r = { Value = a; Left = l; Right = r }
let fromTree t = { Focus = t; Path = [] }

let up z =
    // Reconstruct the tree on the way up ->
    // since the focus can set it can be different than
    // the "history" in the path; by setting either the
    // left or right with the current state we align these.
    match z.Path with
    | [] -> None
    | (Left, parent) :: ancestors ->
        { Focus = { parent with Left = Some z.Focus }
          Path = ancestors }
        |> Some
    | (Right, parent) :: ancestors ->
        { Focus = { parent with Right = Some z.Focus }
          Path = ancestors }
        |> Some

let rec toTree z =
    match up z with
    | None -> z.Focus
    | Some z' -> toTree z'

let left z =
    z.Focus.Left
    |> Option.map (fun l ->
        { Focus = l
          Path = (Left, z.Focus) :: z.Path })

let right z =
    z.Focus.Right
    |> Option.map (fun r ->
        { Focus = r
          Path = (Right, z.Focus) :: z.Path })

let value (z: 'a Zipper) = z.Focus.Value

let setValue a z =
    { z with
        Focus = { z.Focus with Value = a } }

let setLeft l z =
    { z with
        Focus = { z.Focus with Left = l } }

let setRight r z =
    { z with
        Focus = { z.Focus with Right = r } }
