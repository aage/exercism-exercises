module Yacht

type Category =
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six

let private points =
    function
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6

let private count die dice =
    dice |> List.filter ((=) die) |> List.sumBy points

let private pattern dice =
    dice
    |> List.groupBy id
    |> List.map (snd >> List.length)
    |> List.sort

let private sortByCount dice =
    dice
    |> List.groupBy id
    |> List.sortByDescending (snd >> List.length)
    |> List.collect snd

let score category dice =

    match category with
    | Ones -> count One dice
    | Twos -> count Two dice
    | Threes -> count Three dice
    | Fours -> count Four dice
    | Fives -> count Five dice
    | Sixes -> count Six dice
    | FullHouse ->
        if pattern dice |> (=) [ 2; 3 ]
        then dice |> List.sumBy points
        else 0
    | FourOfAKind ->
        let pattern = pattern dice

        if pattern = [ 5 ] || pattern = [ 1; 4 ]
        then
            sortByCount dice
            |> List.take 4
            |> List.sumBy points
        else 0
    | LittleStraight ->
        if List.sort dice
           |> List.map points
           |> (=) [ 1 .. 5 ]
        then 30
        else 0
    | BigStraight ->
        if List.sort dice
           |> List.map points
           |> (=) [ 2 .. 6 ]
        then 30
        else 0
    | Choice -> dice |> List.sumBy points
    | Yacht ->
        if pattern dice |> (=) [ 5 ]
        then 50
        else 0
