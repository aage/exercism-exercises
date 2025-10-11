module LensPerson

open System
open Aether.Operators

type Person =
    { name: Name
      born: Born
      address: Address }

    static member Born_ =
        (fun p -> p.born), (fun b p -> { p with born = b })

    static member Address_ =
        (fun p -> p.address), (fun a p -> { p with address = a })

and Born =
    { at: Address
      on: DateTime }

    static member At_ =
        (fun b -> b.at), (fun a b -> { b with at = a })

    static member On_ =
        (fun b -> b.on), (fun o b -> { b with on = o })

    static member Month_ =
        (fun b -> b.on.Month),
        (fun m b ->
            { b with
                  on = DateTime(b.on.Year, m, b.on.Day) })

and Address =
    { street: string
      houseNumber: int
      place: string
      country: string }

    static member Street_ =
        (fun a -> a.street), (fun s a -> { a with street = s })

and Name = { name: string; surName: string }

let bornAtStreet =
    Person.Born_ >-> Born.At_ >-> Address.Street_

let currentStreet = Person.Address_ >-> Address.Street_

let bornOn = Person.Born_ >-> Born.On_

let birthMonth = Person.Born_ >-> Born.Month_
