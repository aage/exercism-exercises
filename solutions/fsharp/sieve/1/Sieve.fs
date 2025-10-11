module Sieve

let primes limit =
    match limit < 2 with
    | true -> []
    | false ->

        // take the next available unmarked number in your list (it is prime)
        // mark all the multiples of that number (they are not prime)
        let rec inner nums primes =
            match nums with
            | [] -> List.rev primes
            | head::tail ->
                let prime = primes
                            |> List.exists (fun p -> head % p = 0)
                            |> not
                if prime then
                    inner tail (head :: primes)
                else
                    inner tail primes

        inner [2..limit] []