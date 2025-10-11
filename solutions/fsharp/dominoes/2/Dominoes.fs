module Dominoes

type Domino = int * int

let private connect (lhs: Domino) (rhs: Domino) =

    let flip (domino: Domino) : Domino = (snd domino, fst domino)

    match (snd lhs = fst rhs, snd lhs = snd rhs) with
    | (true, _) -> Some rhs
    | (_, true) -> Some(flip rhs)
    | _ -> None

let private loops (input: Domino list) : bool =

    match input with
    | [] -> true
    | _ ->
        let fst = List.head input |> fst
        let snd = List.last input |> snd
        fst = snd

let private moveToBack (input: Domino list) count =

    List.skip count input @ List.take count input

let rec private chains (acc: Domino list) (dominoes: Domino list) =
    match (acc, dominoes) with
    | (_, []) -> loops acc
    | ([], head :: tail) -> chains [ head ] tail
    | _ ->
        let connect' = connect (List.last acc)

        let data =
            dominoes
            |> List.mapi (fun idx dom -> (idx, dom, connect' dom))

        let nextOpt =
            data
            |> List.tryFind (fun (_, _, opt) -> Option.isSome opt)

        match nextOpt with
        | None -> false
        | Some next ->
            let tail =
                data
                |> List.except (seq { next })
                |> List.map (fun (_, dom, _) -> dom)

            let (_, _, opt) = next
            chains (acc @ [ Option.get opt ]) tail

let canChain (input: Domino list) : bool =

    let rec loop (c: bool) (indices: int list) =
        match (c, indices) with
        | (true, _) -> c
        | (_, []) -> c
        | (_, idx :: indices) ->
            let dominoes = moveToBack input idx
            loop (chains [] dominoes) indices

    match input with
    | [] -> true
    | [ x ] -> loops [ x ]
    | _ -> [ 0 .. List.length input - 1 ] |> loop false
