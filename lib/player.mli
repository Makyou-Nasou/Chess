open Global
open Piece

type strategy = {
  choose_move : color -> piece option list list -> move;
  choose_accept_draw : color -> piece option list list -> bool;
  choose_promotion : color -> piece option list list -> shape;
}

type player

val init_player : color -> strategy -> player
val get_color_from_player : player -> color
val get_choose_move_from_player : player -> piece option list list -> move
val get_choose_accept_draw : player -> piece option list list -> bool
val get_choose_promotion : player -> piece option list list -> shape
val default_strategy : unit -> strategy
