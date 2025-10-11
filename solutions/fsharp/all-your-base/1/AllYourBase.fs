module AllYourBase

let rebase digits inputBase outputBase =

    if(inputBase < 2 || outputBase < 2) then None
    elif(List.exists (fun digit -> digit >= inputBase) digits) then None
    elif(List.exists (fun digit -> digit < 0) digits) then None
    else

        // convert to base 10
        let base10 = digits
                     |> List.rev
                     |> List.mapi (fun index digit -> digit * (pown inputBase index))
                     |> List.sum
                    
        // convert to requested base
        let rec loop (base10:int) (outputBase: int) (digits: int list) =

            match base10 with
            | 0 -> digits
            | _ ->
                let remainder = base10 % outputBase
                let times     = base10 / outputBase
                let newDigits = remainder :: digits

                loop times outputBase newDigits

        let baseOutput = loop base10 outputBase []

        // return as some
        match baseOutput with
        | [] -> Some [0]
        | _  -> Some baseOutput