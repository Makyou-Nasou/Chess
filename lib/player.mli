open Global
open Piece

type strategy = {
  choose_move : piece option array array -> move;
  choose_accept_draw : piece option array array -> bool;
  choose_promotion : piece option array array -> shape;
}

type player

val init_player : color -> strategy -> player
val set_last_move_to_player : player -> move -> player
val get_color_from_player : player -> color
val get_last_move_from_player : player -> move option
val get_choose_move_from_player : player -> piece option array array -> move
val get_choose_accept_draw : player -> piece option array array -> bool
val get_choose_promotion : player -> piece option array array -> shape
val default_strategy : unit -> strategy
