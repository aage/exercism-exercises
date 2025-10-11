module PalindromeProducts

let isPalindrome (num:int) =

    let str = string num

    let rec inner (s:string) =

        let len = String.length s
        match len < 2 with
        | true -> true
        | false ->
            if s.[0] = s.[len - 1]
            then inner s.[1 .. len - 2]
            else false

    inner str

let private smallestCandidates min max =
    let mutable smallestCandidate  : int option = None
    seq {
        let mutable skipTheRestOfA = false

        for a in seq { min .. max } do
            if (not smallestCandidate.IsNone) && smallestCandidate.Value < (a * a)
            then skipTheRestOfA <- true

            if not skipTheRestOfA then
                let mutable skipTheRestOfB = false
                for b in seq { a .. max } do
                    if not skipTheRestOfB then               
                        let product = a * b
                        if smallestCandidate.IsNone || smallestCandidate.Value >= product then
                            if isPalindrome product then
                                smallestCandidate <- Some product
                                yield product
                        elif not smallestCandidate.IsNone then
                            skipTheRestOfB <- true
    }

let private largestCandidates min max =
    let mutable largestCandidate  : int option = None
    seq {
        let mutable skipTheRestOfA = false

        for a in seq { max .. -1 .. min} do
            if (not largestCandidate.IsNone) && largestCandidate.Value > (a * max)
            then skipTheRestOfA <- true

            if not skipTheRestOfA then
                let mutable skipTheRestOfB = false
                for b in seq { max .. -1 .. a } do
                    if not skipTheRestOfB then               
                        let product = a * b
                        if largestCandidate.IsNone || largestCandidate.Value <= product then
                            if isPalindrome product then
                                largestCandidate <- Some product
                                yield product
                        elif not largestCandidate.IsNone then
                            skipTheRestOfB <- true
    }

let factors min max (num:int) =

    let sqr = (float >> sqrt >> int) num
    let outer = System.Math.Max(sqr, max) // check if square root is smaller than max
    let mutable fs = []
    [ for fst in min .. outer do
       if (num % fst = 0) then
        let snd = num / fst
        if (min <= snd && snd <= outer) then
         fs <- (fst, snd) :: fs
         if (List.contains (snd, fst) fs |> not || fst = snd) then yield (fst, snd) ]

let private findPalindromes min max smallToLarge =
    
    let sortByProjection = if smallToLarge then (fun p -> p) else (fun p -> -p)
    let bestCandidates = if smallToLarge then smallestCandidates else largestCandidates
    
    bestCandidates min max
    |> Seq.sortBy sortByProjection
    |> Seq.tryHead
    |> Option.map (fun (p) ->
        (Some p, factors min max p))
    |> Option.defaultValue (None, [])

let largest minFactor maxFactor =
    if minFactor > maxFactor then raise (System.ArgumentException())
    findPalindromes minFactor maxFactor false

let smallest minFactor maxFactor =
    if minFactor > maxFactor then raise (System.ArgumentException())
    findPalindromes minFactor maxFactor true