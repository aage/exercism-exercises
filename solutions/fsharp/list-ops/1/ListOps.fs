module ListOps

let rec foldl folder state list =
    let mutable acc = state
    for x in list do
        acc <- folder acc x
    acc

let rec foldr folder state list =
    let mutable acc = state
    for x in list do
        acc <- folder x acc
    acc

let length list =
    let mutable len = 0
    for _ in list do
        len <- len + 1
    len

let reverse list =
    let mutable xs = []
    for x in list do
        xs <- x::xs
    xs

let map f list =
    let mutable xs = []
    for x in list do
        xs <- xs @ [f(x)]
    xs

let filter f list =
    let mutable xs = []
    for x in list do
        if f(x)
        then xs <- xs @ [x]
    xs

let append xs ys =
    let mutable zs = []
    for x in xs do
        zs <- zs @ [x]
    for y in ys do
        zs <- zs @ [y]
    zs

let concat xs =
    let mutable zs = []
    for ys in xs do
        for y in ys do
            zs <- zs @ [y]
    zs