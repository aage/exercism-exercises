module AllYourBase

let baseToDecimal b digits = 
    digits
    |> List.rev
    |> List.mapi (fun i d -> d * (pown b i))
    |> List.sum

let decimalToBase b num =

    let rec loop num digits =
        match num with
        | 0 -> digits
        | _ -> loop (num / b) (num % b :: digits) 

    match num with
    | 0 -> [0]
    | _  -> loop num [] 

let rebase digits inputBase outputBase =

    let validInput = inputBase > 1
                     && outputBase > 1
                     && List.forall (fun digit -> digit >= 0 && digit < inputBase) digits

    match validInput with
    | false -> None
    | true  -> 
        let num = baseToDecimal inputBase digits
        let output = decimalToBase outputBase num
        Some output