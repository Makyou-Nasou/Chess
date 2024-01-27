type coordinates = int * int

exception Invalide_coordinates

val is_valide_coordinates : coordinates -> bool

type move =
  | Movement of coordinates * coordinates
  | Small_Castling
  | Big_Castling

type color = Black | White
