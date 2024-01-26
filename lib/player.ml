open Piece
open Global

type player = {
  color : color;
  last_move : (piece * coordonne * coordonne) option;
}

let init_player c = { color = c; last_move = None }
