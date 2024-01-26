open Piece
open Global

type player = {
  color : color;
  last_move : (piece * coordonne * coordonne) option;
}

val init_player : color -> player
