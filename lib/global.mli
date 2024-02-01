type coordinates = int * int

exception Invalid_coordinates

(*Check if a coordinate is in the board.*)
val is_valid_coordinates : coordinates -> bool

type move =
  | Movement of coordinates * coordinates
  | Small_Castling
  | Big_Castling
  | Propose_Draw
  | Give_Up

type color = Black | White

val get_other_color : color -> color
val convert_coordinates : string -> coordinates
