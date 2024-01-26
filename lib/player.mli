open Global
open Board

type player

val init_player : (board -> move) -> color -> player
val get_color_from_player : player -> color
val get_last_move_from_player : player -> move option
val get_choose_move_from_player : player -> board -> move
val default_choose_move : board -> move
