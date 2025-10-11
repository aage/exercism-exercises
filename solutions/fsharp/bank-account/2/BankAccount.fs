module BankAccount

open System.Collections.Concurrent

type AccountState = | Closed | Open

type Account () =
    
    let changes:ConcurrentBag<decimal> = ConcurrentBag<decimal>()
    
    member val State = Closed with get,set
    member this.Balance =
        match changes.IsEmpty with
        | true -> 0.0m
        | false ->
            let balance = changes |> Seq.reduce (+)
            balance
    member this.Update msg =
        changes.Add msg

let mkBankAccount() = Account()

let openAccount (account:Account) =
    account.State <- Open
    account

let closeAccount (account:Account) =
    account.State <- Closed
    account

let getBalance (account:Account) =
    match account.State with
    | Closed -> None
    | Open -> account.Balance |> Some

let updateBalance change (account:Account) =
    account.Update change
    account