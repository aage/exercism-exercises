module SpaceAge

type Planet =
 | Earth
 | Mercury
 | Venus
 | Mars
 | Jupiter
 | Saturn
 | Uranus
 | Neptune

let (|EarthYearsPerPlanetYear|) p =
    match p with
    | Earth -> 1.0
    | Mercury -> 0.2408467
    | Venus -> 0.61519726
    | Mars -> 1.8808158
    | Jupiter -> 11.862615
    | Saturn -> 29.447498
    | Uranus -> 84.016846
    | Neptune -> 164.79132

let age (planet: Planet) (seconds: int64): float =
    
    let secondsToEarthYear (seconds: int64) =
        let yearInDays = 365.25
        seconds
        |> float
        |> fun s -> s / 60. / 60. / 24. / yearInDays
    
    let earthYearToPlanetYear (earthYear:float) =
        let (EarthYearsPerPlanetYear(factor)) = planet
        earthYear / factor
    
    seconds |> secondsToEarthYear |> earthYearToPlanetYear