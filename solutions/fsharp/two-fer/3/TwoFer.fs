module TwoFer

let twoFer (input: string option): string =
    let name = Option.defaultValue "you" input
    let message = sprintf "One for %s, one for me." name
    message