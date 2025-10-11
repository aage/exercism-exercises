module Triangle

let private validate (triangle: float list) f =

    let (a, b, c) = (triangle[0], triangle[1], triangle[2])

    match [ a; b; c ] |> List.filter ((>=) 0) with
    | [] ->
        if a + b < c then false
        elif b + b < a then false
        elif a + c < b then false
        else f a b c
    | _ -> false

let equilateral triangle =

    let equilateral a b c = (a = b && b = c)
    validate triangle equilateral

let isosceles triangle =

    let isosceles a b c = (a = b || a = c || b = c)
    validate triangle isosceles

let scalene triangle =

    let scalene a b c = (a <> b && a <> c && b <> c)
    validate triangle scalene
