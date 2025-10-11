module SimpleLinkedList

type Element = { Value: int; Next: Element option }

let private value (e: Element) = e.Value

let nil: Element option = None

let create (x: int) (n: Element option) : Element option = Some { Value = x; Next = n }

let isNil (opt: Element option) = Option.isNone opt

let next (opt: Element option) = opt |> Option.bind (fun e -> e.Next)

let datum (opt: Element option) =
    opt
    |> Option.map (fun e -> e.Value)
    |> Option.defaultValue 0

let toList (opt: Element option) =
    let mutable (next: Element option) = opt

    [ while Option.isSome next do
          yield
              next
              |> Option.map
                  (fun e ->
                      next <- e.Next
                      e)
              |> Option.map value
              |> Option.get ]

let fromList (xs: int list) =
    List.rev xs
    |> List.map (fun v -> create v None)
    |> List.reduce
        (fun n x ->
            x
            |> Option.map value
            |> Option.get
            |> fun v -> create v n)

let reverse x = toList x |> List.rev |> fromList
