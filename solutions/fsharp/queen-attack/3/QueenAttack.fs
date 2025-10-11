module QueenAttack

let create (position: int * int) =
   let onBoard n = 0 <= n && n <= 7
   let column, row = position
   
   onBoard column && onBoard row

let distance (lhs:int) (rhs:int) = abs (lhs - rhs)

let canAttack (queen1: int * int) (queen2: int * int) =
   let row1, column1 = queen1
   let row2, column2 = queen2
   
   let attacksOnRow =
      row1 = row2
   let attacksOnColumn =
      column1 = column2
   let attacksOnDiagonal =
      let distanceX = distance row1 row2
      let distanceY = distance column1 column2
      distanceX = distanceY

   attacksOnRow || attacksOnColumn || attacksOnDiagonal