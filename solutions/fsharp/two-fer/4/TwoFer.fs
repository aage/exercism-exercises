module TwoFer

let twoFer (input: string option): string =
    Option.defaultValue "you" input
    |> sprintf "One for %s, one for me."