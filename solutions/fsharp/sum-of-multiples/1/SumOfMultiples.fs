module SumOfMultiples

let sum (numbers: int list) (upperBound: int) : int =

    let ``is multiple`` n =
        numbers
        |> List.exists (fun num -> num > 0 && n % num = 0)

    [ for num in [ 1 .. upperBound - 1 ] do
          if ``is multiple`` num then yield num ]
    |> List.sum
