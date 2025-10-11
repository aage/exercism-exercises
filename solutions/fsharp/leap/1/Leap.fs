module Leap

let leapYear (year: int): bool =
    let evenlyDivisbleBy = fun x -> year % x = 0

    let divBy4      = evenlyDivisbleBy 4
    let divBy100    = evenlyDivisbleBy 100
    let divBy400    = evenlyDivisbleBy 400
    
    (divBy4 && not divBy100) || divBy400