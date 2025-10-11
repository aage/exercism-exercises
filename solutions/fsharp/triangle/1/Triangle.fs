module Triangle

let private validate (triangle: float list) =

    let (a, b, c) = (triangle[0], triangle[1], triangle[2])

    match [ a; b; c ] |> List.filter ((>=) 0) with
    | [] ->
        if a + b < c then Error "NoNo"
        elif b + b < a then Error "NoNo"
        elif a + c < b then Error "NoNo"
        else Ok(a, b, c)
    | _ -> Error "NoNo"

let equilateral triangle =

    validate triangle
    |> Result.bind (fun (a, b, c) ->
        if a = b && b = c
        then Ok(a, b, c)
        else Error "No equilateral")
    |> function
        | Ok _ -> true
        | Error e -> false

let isosceles triangle =

    validate triangle
    |> Result.bind (fun (a, b, c) ->
        if a = b || a = c || b = c
        then Ok(a, b, c)
        else Error "No isosceles")
    |> function
        | Ok _ -> true
        | Error e -> false

let scalene triangle =
    validate triangle
    |> Result.bind (fun (a, b, c) ->
        if a <> b && a <> c && b <> c
        then Ok(a, b, c)
        else Error "No scalene")
    |> function
        | Ok _ -> true
        | Error e -> false
