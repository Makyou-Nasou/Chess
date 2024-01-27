open Global
open Piece

type player

val init_player : (piece option array array -> move option) -> color -> player
val set_last_move_to_player : player -> move -> player
val get_color_from_player : player -> color
val get_last_move_from_player : player -> move option

val get_choose_move_from_player :
  player -> piece option array array -> move option

val default_choose_move : piece option array array -> move option
