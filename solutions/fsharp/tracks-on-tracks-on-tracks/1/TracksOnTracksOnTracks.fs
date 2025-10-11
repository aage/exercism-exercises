module TracksOnTracksOnTracks

let newList: string list = []

let existingList: string list = [ "F#"; "Clojure"; "Haskell" ]

let addLanguage (language: string) (languages: string list) : string list = language :: languages

let countLanguages (languages: string list) : int =
    let mutable count = 0

    for _ in languages do
        count <- count + 1

    count

let reverseList (languages: string list) : string list =
    let mutable rev = []

    for idx = (List.length languages) - 1 downto 0 do
        rev <- rev @ [ languages.[idx] ]

    rev

let excitingList (languages: string list) : bool =
    match languages with
    | [] -> false
    | x :: xs when x = "F#" -> true
    | [ _; "F#" ] -> true
    | [ _; "F#"; _ ] -> true
    | _ -> false
