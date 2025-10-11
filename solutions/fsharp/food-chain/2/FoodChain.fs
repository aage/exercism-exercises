module FoodChain

let elements =
    [| "", "", None
       "fly", "I don't know why she swallowed the fly. Perhaps she'll die.", None
       "spider", "It wriggled and jiggled and tickled inside her.", Some "She swallowed the spider to catch the fly."
       "bird", "How absurd to swallow a bird!", Some "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
       "cat", "Imagine that, to swallow a cat!", Some "She swallowed the cat to catch the bird."
       "dog", "What a hog, to swallow a dog!", Some "She swallowed the dog to catch the cat."
       "goat", "Just opened her throat and swallowed a goat!", Some "She swallowed the goat to catch the dog."
       "cow", "I don't know how she swallowed a cow!", Some "She swallowed the cow to catch the goat."
       "horse", "She's dead, of course!", None |]

let recite start stop : string list =

    [ start .. stop ]
    |> List.map (fun iteration ->
        if iteration = 8 then
            let (_, comment, _) = elements.[8]
            let lines = [ "I know an old lady who swallowed a horse."; comment ]
            lines
        else
            [ iteration .. -1 .. 1 ]
            |> List.collect (fun verse ->
                let (animal, comment, explanation) = elements.[verse]
                [ if verse = iteration then yield sprintf "I know an old lady who swallowed a %s." animal
                  if verse = iteration || verse = 1 then yield comment
                  if explanation.IsSome then yield explanation.Value ]))

    |> List.collect (fun c -> c @ [ "" ]) // intersperse list
    |> List.rev |> List.tail |> List.rev // remove last empty string