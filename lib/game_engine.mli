open Player
open Global
open Board

type game

val init_game : strategy -> strategy -> game
val pp_game : Format.formatter -> game -> unit
val get_current_player_from_game : game -> player
val get_next_player_from_game : game -> player
val get_board_from_game : game -> board
val play_move : game -> move -> game option
val start_game : strategy -> strategy -> results
