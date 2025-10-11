module Bowling

type private Frame =
    | Open of (int * int)
    | Spare of (int * int)
    | Strike
    | Fill of int

type private Game = | Game of Frame list

let private pins (frame: Frame) =
    match frame with
    | Fill s -> [s]
    | Strike -> [10]
    | Spare (fst,snd) -> [fst;snd]
    | Open (fst,snd) -> [fst;snd]

let private frames (game: int list) =

    let rec loop (count:int) (acc: Frame option list) (data: int list) =
        match data with
        | [] -> acc |> List.rev
        | this::tail ->
            match (this, count) with
            | _ when this < 0 || this > 10 || count > 12 -> [None]
            | (_, 11) ->
                match List.head acc with
                | Some(Strike) -> loop (count + 1) (Some(Fill this)::acc) tail
                | Some(Spare(_)) -> loop (count + 1) (Some(Fill this)::acc) tail
                | _ -> [None]
            | (_, 12) ->
                match (List.item 1 acc, List.head acc) with
                | (Some(Strike), Some(Fill last))
                    when last = 10 || this <> 10 && this + last < 11
                    -> loop (count + 1) (Some(Fill this)::acc) tail
                | _ -> [None]
            | (10, _) -> loop (count + 1) (Some(Strike)::acc) tail
            | _ ->
                match (this, tail[0], tail[0] + this) with
                | (_, _, sum) when sum > 10 -> [None]
                | (_, snd, 10) -> loop (count + 1) (Some(Spare(this, snd))::acc) tail[1..]
                | (_, snd, _) -> loop (count + 1) (Some(Open(this, snd))::acc) tail[1..]

    game |> loop 1 []

let private createGame (frames: Frame list) =

    match List.length frames with
    | len when len < 10 -> None
    | len when len = 10 ->
        match List.last frames with
        | Spare _ -> None
        | Strike -> None
        | _ -> frames |> Game |> Some
    | len when len = 11 ->
        match frames[9] with
        | Strike -> None
        | _ -> frames |> Game |> Some
    | _ -> frames |> Game |> Some

let newGame () = []

let roll pins rolls = rolls @ [pins]

let score (game: int list) =

    let rec fold (sum:int) (acc:Frame list) (frames: Frame list) =
        match frames with
        | [] -> sum
        | head::tail ->
            match (head, List.tryHead acc, List.tryItem 1 acc) with
            | (Fill _, _, _) -> fold sum (head::acc) tail
            | (Open (fst, snd), _, _) -> fold (sum + fst + snd) (head::acc) tail
            | (Spare (fst, snd), Some f1, _) ->
                let bonus = pins f1 |> List.head
                fold (sum + fst + snd + bonus) (head::acc) tail
            | (Strike, Some f1, Some f2) ->
                let bonus = [f1;f2] |> List.collect pins |> List.take 2 |> List.sum
                fold (sum + 10 + bonus) (head::acc) tail
            | _ -> fold sum (head::acc) tail

    frames game
    |> List.choose id
    |> createGame
    |> Option.map (fun (Game frames) ->
        List.rev frames |> fold 0 [])