module NthPrime

// taken from: https://stackoverflow.com/a/35995297/2877982
let rec primes = 
    seq {
        yield 2
        let mutable x = 3
        while true do
            if isprime x then 
                yield x
            x <- x + 2
    }
and isprime x =
    use e = primes.GetEnumerator()
    let rec loop() =
        if e.MoveNext() then
            let p = e.Current
            if p * p > x then true
            elif x % p = 0 then false
            else loop()
        else true            
    loop()

let prime nth : int option =
    if (nth < 1)
    then None
    else primes |> Seq.item (nth - 1) |> Some