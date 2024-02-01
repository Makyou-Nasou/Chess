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

let get_other_color c = match c with Black -> White | White -> Black

let convert_coordinates (coord : string) : int * int =
  if String.length coord > 2 then raise Invalid_coordinates
  else
    let convert_char c = Char.code c - Char.code 'a' in
    let convert_chiffre c = Char.code '8' - Char.code c in
    let y = convert_char coord.[0] in
    let x = convert_chiffre coord.[1] in
    if is_valid_coordinates (x, y) then (x, y) else raise Invalid_coordinates
