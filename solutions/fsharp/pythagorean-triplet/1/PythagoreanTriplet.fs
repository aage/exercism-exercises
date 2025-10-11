module PythagoreanTriplet

let tripletsWithSum sum =

    let is triplet =
        let (a, b, c) = triplet
        let aa = a * a
        let bb = b * b
        let cc = c * c

        aa + bb = cc && aa < bb && bb < cc

    // a should be in the first third
    [ for num in [ 1 .. sum / 3 ] do
          // b should be between that and the half of the sum
          for other in [ num + 1 .. sum / 2 ] do
              // yield if is triplet
              let triplet = (num, other, sum - (num + other))
              if is triplet then yield triplet ]
