module LinkedList

type Lst = { mutable nodes: int list }

let private rev (lst: Lst) = { nodes = lst.nodes |> List.rev }

let private tailHead (lst: Lst) =
    let nodes = lst.nodes
    (List.head nodes, List.tail nodes)

let mkLinkedList () = { nodes = [] }

let pop (lst: Lst) =
    let (head, tail) = tailHead lst
    lst.nodes <- tail
    head

let shift (lst: Lst) =

    let (head, tail: int list) = rev lst |> tailHead
    lst.nodes <- List.rev tail
    head

let push newValue (lst: Lst) =
    lst.nodes <- newValue :: lst.nodes
    ()

let unshift newValue (linkedList: Lst) =
    linkedList.nodes <- linkedList.nodes @ [ newValue ]
    ()
