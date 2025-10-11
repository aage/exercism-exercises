module BankAccount

open System

let monitor = Object()
let log msg =
    lock monitor (fun () ->
        System.IO.File.AppendAllLines(@"c:\temp\async-bank.log", [msg]))

type AccountState = | Closed | Open

type Msg =
    | Change of decimal
    | Fetch of AsyncReplyChannel<decimal>

type Account () =
    
    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async { let! msg = inbox.Receive()
                        match msg with
                        | Change(x) ->
                            return! loop(n + x)
                        | Fetch(replyChannel) ->
                            replyChannel.Reply(n)
                            return! loop(n) }
            loop 0.0m)

    member val State = Closed with get,set
    member val Balance = 0.0m with get,set
    member this.Post msg = agent.Post msg
    member this.PostAndReply = agent.PostAndReply

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
    sprintf "Updating account balance %f with change %f" account.Balance change |> log
    account.Post(Change change)
    let balance = account.PostAndReply(Fetch)
    account.Balance <- balance
    sprintf "Balance now is %f" account.Balance |> log
    account