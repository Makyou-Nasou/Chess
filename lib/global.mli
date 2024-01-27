type coordinates = int * int

exception Invalide_coordinates

val is_valide_coordinates : coordinates -> bool

type move =
  | Movement of coordinates * coordinates
  | Small_Castling
  | Big_Castling
  | Propose_Draw
  | Give_Up

type color = Black | White
type results = Winner of color | Draw
