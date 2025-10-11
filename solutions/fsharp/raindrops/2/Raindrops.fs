module Raindrops

let convert (number: int): string =

    let evenlyDivisible div x = x % div = 0

    let factorSoundPairs =
        [
            (3, "Pling")
            (5, "Plang")
            (7, "Plong")
        ]

    let sounds =
        factorSoundPairs
         |> List.filter (fun (factor, _) -> evenlyDivisible factor number)
         |> List.map    (fun (_, sound)  -> sound)
    
    if List.isEmpty sounds
    then number.ToString()
    else sounds |> List.reduce (fun x y -> sprintf "%s%s" x y) 