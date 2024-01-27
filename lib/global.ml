type coordinates = int * int

type move =
  | Movement of coordinates * coordinates
  | Small_Castling
  | Big_Castling

type color = Black | White
