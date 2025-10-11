module Allergies

open System

type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let allergicTo codedAllergies (allergen: Allergen) =

    int allergen &&& codedAllergies |> ((=) (int allergen))

let list codedAllergies =

    let allergens = Enum.GetValues(typeof<Allergen>) :?> Allergen [] |> Array.toList

    let allergicTo' = allergicTo codedAllergies
    [ for allergen in allergens do
        if allergicTo' allergen then yield allergen ]
