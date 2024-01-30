open Global
open Piece
open Board

type strategy = {
  choose_move : color -> board -> move;
  choose_accept_draw : color -> board -> bool;
  choose_promotion : color -> board -> shape;
}

type player

(*Method that initializes a player thanks to his color and strategy.*)
val init_player : color -> strategy -> player

(*Returns the color of the indicated player.*)
val get_color_from_player : player -> color

(*Returns the strategy to choose a player’s movement.*)
val get_choose_move_from_player : player -> board -> move

(*Returns the strategy to choose draw or not.*)
val get_choose_accept_draw : player -> board -> bool

(*Returns the strategy to choose a piece promotion.*)
val get_choose_promotion : player -> board -> shape

(*Default policy for responding in the terminal.*)
val default_strategy : unit -> strategy
