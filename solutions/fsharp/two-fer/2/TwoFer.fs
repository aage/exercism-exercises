module TwoFer

let twoFer (input: string option): string =
    let name = Option.defaultValue "you" input
    let greeting = sprintf "One for %s, one for me." name
    greeting