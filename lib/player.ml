open Global
open Board

type player = {
  color : color;
  last_move : move option;
  choose_move : board -> move;
}

let init_player choose_move c = { color = c; last_move = None; choose_move }
let get_color_from_player (p : player) = p.color
let get_last_move_from_player (p : player) = p.last_move
let get_choose_move_from_player (p : player) = p.choose_move
let default_choose_move b = if b = b then Small_Castling else Small_Castling
