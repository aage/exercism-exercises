module Seq

let keep pred (xs: seq<'a>) =
    seq { for x in xs do if pred x then x }

let discard pred (xs: seq<'a>) =
    seq { for x in xs do if not (pred x) then x }