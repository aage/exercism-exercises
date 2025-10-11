module PythagoreanTriplet

let private square n = n * n

let tripletsWithSum sum =

    let pythagoreanTriplet (triplet: int * int * int) =

        let (a, b, c) = triplet
        match (a < b, b < c) with
        | (true, true) ->
            square a + square b = square c
        | _ -> false

    [ for num in [ 1 .. (sum / 3) ] do
          for other in [ num + 1 .. (sum / 2) ] do
              let triplet = (num, other, sum - (num + other))
              if pythagoreanTriplet triplet then yield triplet ]
