module PrimeFactors

open System

let private isPrime (n: int) =

    let sq = Math.Sqrt n |> int

    seq {

        for div in 2..sq do
            if n % div = 0 then
                yield false
    }
    |> Seq.tryHead
    |> Option.defaultValue true

let factors (number: int64) =

    let primes = (Seq.initInfinite ((+) 2) |> Seq.filter isPrime).GetEnumerator()

    let mutable rest = number

    [ while rest > 1 do
          primes.MoveNext() |> ignore
          let prime = primes.Current

          while rest % int64 prime = 0 do
              yield prime
              rest <- rest / int64 prime ]
