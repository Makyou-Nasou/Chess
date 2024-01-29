type coordinates = int * int

exception Invalid_coordinates

let is_valid_coordinates (l, c) = l <= 7 && l >= 0 && c <= 7 && c >= 0

type move =
  | Movement of coordinates * coordinates
  | Small_Castling
  | Big_Castling
  | Propose_Draw
  | Give_Up

type color = Black | White
type results = Winner of color | Draw | Error of string
