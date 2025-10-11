module SpaceAge

[<Measure>] type second
[<Measure>] type year

type Planet =
 | Earth
 | Mercury
 | Venus
 | Mars
 | Jupiter
 | Saturn
 | Uranus
 | Neptune

let (|EarthYearsPerPlanetYear|) orbitalPeriod =
    match orbitalPeriod with
    | Earth -> 1.<year>
    | Mercury -> 0.2408467<year>
    | Venus -> 0.61519726<year>
    | Mars -> 1.8808158<year>
    | Jupiter -> 11.862615<year>
    | Saturn -> 29.447498<year>
    | Uranus -> 84.016846<year>
    | Neptune -> 164.79132<year>

let age (planet: Planet) (seconds: int64): float =
    
    let earthYear = float seconds / 31557600.</second>
    let planetYear =
        let (EarthYearsPerPlanetYear(factor)) = planet
        earthYear / factor

    float planetYear