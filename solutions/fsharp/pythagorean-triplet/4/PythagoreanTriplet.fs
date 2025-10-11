module PythagoreanTriplet

let private square n = n * n

let tripletsWithSum sum =

    [ for a in [ 1 .. (sum / 3) ] do
          for b in [ a + 1 .. (sum / 2) ] do
              let c = sum - a - b

              if square a + square b = square c then
                  yield (a, b, c) ]
