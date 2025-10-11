module DndCharacter

open System

type Character =
    { Strength: int
      Dexterity: int
      Constitution: int
      Intelligence: int
      Wisdom: int
      Charisma: int
      Hitpoints: int }

let private r = Random()
let private rollDice () = r.Next(1, 6)

let modifier x =
    (float x) - 10. |> (*) 0.5 |> Math.Floor |> int

let ability () =
    let numberOfRolls = 4

    let rolls =
        [ for _ in 1..numberOfRolls do
              yield rollDice () ]

    List.sum rolls - List.min rolls

let createCharacter () =
    let constitution = ability ()

    { Strength = ability ()
      Dexterity = ability ()
      Constitution = constitution
      Intelligence = ability ()
      Wisdom = ability ()
      Charisma = ability ()
      Hitpoints = 10 + (modifier constitution) }
