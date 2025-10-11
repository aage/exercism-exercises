module AtbashCipher

open System.Text.RegularExpressions

let private alphabet = ['a' .. 'z']
let private normalize str = Regex.Replace(str, "[\s,\.]+", "").ToLower()

let private inner (str:string) (delimiterOpt: (int * string) option) =

    normalize str
    |> Seq.mapi (fun idx c -> (c, idx))
    |> Seq.fold (
        fun acc data ->
            let (c, index) = data
            let delimiter =
                delimiterOpt
                |> Option.map (fun (idx, del) -> if index % idx = 0 then del else "")
                |> Option.defaultValue ""

            let translated =
                match List.tryFindIndex (fun ch -> ch = c) alphabet with
                | None -> c
                | Some i -> alphabet.[List.length alphabet - i - 1]
            sprintf "%s%s%c" acc delimiter translated) ""
    |> fun str -> str.Trim()

let encode str = Some (5, " ") |> inner str

let decode str = None |> inner str