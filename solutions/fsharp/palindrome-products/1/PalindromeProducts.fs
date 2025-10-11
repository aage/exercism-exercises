module PalindromeProducts

let log msg = System.IO.File.AppendAllLines(@"c:\temp\palindrome.log", [|msg|])

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

let palindromeProducts min max flip =
    
    let lst = [min .. max]
    let len = List.length lst
    let ``10 percent`` = len / 10
    let nums = if flip then List.rev lst else lst

    let palindromes =
        [for x in 0 .. ``10 percent`` do
          for y in 0 .. len - 1 do
           let p = nums.[x] * nums.[y]
           if isPalindrome p then p ]
    
    if flip
    then palindromes |> List.sortByDescending id |> List.tryHead 
    else palindromes |> List.sortBy id |> List.tryHead 


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

let largest min max =

    if min > max then raise (System.ArgumentException())
    else
        let flip = true
        palindromeProducts min max flip
        |> Option.map (fun product -> (product, factors min max product))
        |> fun opt -> match opt with
                      | None -> (None, [])
                      | Some p -> (fst p |> Some, snd p)

let smallest min max =

    if min > max then raise (System.ArgumentException())
    else
        let flip = false
        palindromeProducts min max flip
        |> Option.map (fun product -> (product, factors min max product))
        |> fun opt -> match opt with
                      | None -> (None, [])
                      | Some p -> (fst p |> Some, snd p)