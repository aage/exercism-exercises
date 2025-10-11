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
            | _ when this < 0 || this > 10 || count > 12 -> loop (count + 1) (None::acc) tail[1..]
            | _ when count = 11 ->
                let valid =
                    match List.head acc with
                    | Some(Strike) -> true
                    | Some(Spare(_)) -> true
                    | _ -> false
                if valid
                then loop (count + 1) (Some(Fill this)::acc) tail
                else loop (count + 1) (None::acc) tail
            | _ when count = 12 ->
                let valid =
                    match (acc |> List.item 1, List.head acc) with
                    | (Some(Strike), Some(Fill last)) ->
                        if last = 10
                        then true
                        elif last + this < 11 then true 
                        else false
                    | _ -> false
                if valid
                then
                    let lst =
                        acc
                        |> List.head
                        |>
                        (function
                        | Some f -> pins f |> List.sum
                        | _ -> 0)
                    if this = 10 && lst <> 10
                    then loop (count + 1) (None::acc) tail
                    else loop (count + 1) (Some(Fill this)::acc) tail
                else loop (count + 1) (None::acc) tail
            | (10, _) -> loop (count + 1) (Some(Strike)::acc) tail
            | _ ->
                let snd = tail[0]
                let sum = this + snd
                if sum > 10 then loop (count + 1) (None::acc) tail[1..]
                elif sum = 10
                then loop (count + 1) (Some(Spare(this, snd))::acc) tail[1..]
                else loop (count + 1) (Some(Open(this, snd))::acc) tail[1..]

    game |> loop 1 []

let private createGame (opts: Frame option list) =

    match List.length opts with
    | _ when opts |> List.exists ((=) None) -> None
    | len when len < 10 -> None
    | len when len = 10 ->
        match List.last opts with
        | Some(Spare _) -> None
        | Some(Strike) -> None
        | _ -> opts |> List.choose id |> Game |> Some
    | len when len = 11 ->
        let last = opts[9]
        match last with
        | Some(Strike) -> None
        | _ -> opts |> List.choose id |> Game |> Some
    | _ -> opts |> List.choose id |> Game |> Some

let newGame () = []

let roll pins rolls = rolls @ [pins]

let score (game: int list) =

    let rec fold (sum:int) (acc:Frame list) (frames: Frame list) =
        match frames with
        | [] -> sum
        | head::tail ->
            match (head, List.tryHead acc, List.tryItem 1 acc) with
            | (Fill s, _, _) -> fold (sum + s) (head::acc) tail
            | (Open (fst, snd), _, _) -> fold (sum + fst + snd) (head::acc) tail
            | (Spare (fst, snd), Some(Fill(_)), Some(Fill(_))) ->
                fold (sum + fst + snd) (head::acc) tail
            | (Spare (fst, snd), Some(Fill(_)), _) ->
                fold (sum + fst + snd) (head::acc) tail
            | (Spare (fst, snd), Some f1, _) ->
                let bonus = pins f1 |> List.head
                fold (sum + fst + snd + bonus) (head::acc) tail
            | (Spare (fst, snd), _, _) ->
                fold (sum + fst + snd) (head::acc) tail
            | (Strike, Some(Fill(_)), Some(Fill(_))) ->
                fold (sum + 10) (head::acc) tail
            | (Strike, Some f1, Some(Fill(_))) ->
                let bonus = [f1] |> List.collect pins |> List.take 1 |> List.sum
                fold (sum + 10 + bonus) (head::acc) tail
            | (Strike, Some(Fill(_)), _) ->
                fold (sum + 10) (head::acc) tail
            | (Strike, Some f1, Some f2) ->
                let bonus = [f1;f2] |> List.collect pins |> List.take 2 |> List.sum
                fold (sum + 10 + bonus) (head::acc) tail
            | (Strike, Some f1, _) ->
                let bonus = [f1] |> List.collect pins |> List.take 1 |> List.sum
                fold (sum + 10 + bonus) (head::acc) tail
            | (Strike, _, _) ->
                fold (sum + 10) (head::acc) tail

    frames game
    |> createGame
    |> Option.map (fun (Game frames') ->
        frames' |> List.rev |> fold 0 [])