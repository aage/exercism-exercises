module QueenAttack

open System

let (|OnBoard|_|) (position: int * int) =
   let x = fst position
   let y = snd position
   let onBoard n = n > -1 && n < 8
   if (onBoard x && onBoard y) then Some(position) else None

let create (position: int * int) =
   match position with
   | OnBoard _ -> true
   | _         -> false

let distance (x:int) (y:int) = x - y |> Math.Abs

let canAttack (queen1: int * int) (queen2: int * int) =
   let row     = fst queen1 = fst queen2
   let column  = snd queen1 = snd queen2
   let diagonal = 
      let distanceX = distance (fst queen1) (fst queen2)
      let distanceY = distance (snd queen1) (snd queen2)
      distanceX = distanceY

   row || column || diagonal