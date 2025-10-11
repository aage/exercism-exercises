module FoodChain

type Animal =
    { Name: string
      Deadly: bool
      Exclamation: string
      mutable Inner: Animal option }

let animals =
    dict
        [ 1,
          { Name = "fly"
            Deadly = false
            Exclamation = "I don't know why she swallowed the fly. Perhaps she'll die."
            Inner = None }
          2,
          { Name = "spider"
            Deadly = false
            Exclamation = "It wriggled and jiggled and tickled inside her."
            Inner = None }
          3,
          { Name = "bird"
            Deadly = false
            Exclamation = "How absurd to swallow a bird!"
            Inner = None }
          4,
          { Name = "cat"
            Deadly = false
            Exclamation = "Imagine that, to swallow a cat!"
            Inner = None }
          5,
          { Name = "dog"
            Deadly = false
            Exclamation = "What a hog, to swallow a dog!"
            Inner = None }
          6,
          { Name = "goat"
            Deadly = false
            Exclamation = "Just opened her throat and swallowed a goat!"
            Inner = None }
          7,
          { Name = "cow"
            Deadly = false
            Exclamation = "I don't know how she swallowed a cow!"
            Inner = None }
          8,
          { Name = "horse"
            Deadly = true
            Exclamation = "She's dead, of course!"
            Inner = None } ]

let chain start stop =

    let rec inner last indices =
        match indices with
        | [] -> last
        | head :: tail ->
            let this = animals.[head]
            let animal =
                if not this.Deadly then { this with Inner = last } else this
            inner (Some animal) tail

    let animals =
        [ start .. stop ]
        |> List.map (fun idx ->
            let indices = [ 1 .. idx ]
            let animal = Some animals.[List.head indices]
            inner animal (List.tail indices))

    animals

let verse initial animal innerOpt =

    let formatInner animal inner =
        if inner.Name = "spider" then
            sprintf "She swallowed the %s to catch the %s that wriggled and jiggled and tickled inside her."
                animal.Name inner.Name
        else
            sprintf "She swallowed the %s to catch the %s." animal.Name inner.Name

    [ if initial then
        yield (sprintf "I know an old lady who swallowed a %s." animal.Name)
        yield animal.Exclamation
        if Option.isSome innerOpt then yield formatInner animal (Option.get innerOpt)
      else if Option.isSome innerOpt then
          yield formatInner animal (Option.get innerOpt)
      else
          yield animal.Exclamation ]

let recite start stop: string list =

    let rec inner verses initial animalOpt =
        match animalOpt with
        | None -> verses
        | Some animal ->
            let these = verse initial animal animal.Inner
            let appended = verses @ these
            inner appended false animal.Inner

    let chain = chain start stop

    let verses =
        chain
        |> List.collect (fun c ->
            let v = inner [] true c
            v @ [ "" ]) // intersperse list
        |> List.rev |> List.tail |> List.rev // remove last empty string
    verses
