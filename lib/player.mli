open Piece
open Global

type player

val init_player : color -> player
val get_color_from_player : player -> color
val get_last_move_from_player : player -> move option
